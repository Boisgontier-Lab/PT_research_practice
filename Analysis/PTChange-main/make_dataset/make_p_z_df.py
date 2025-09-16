import os
import re
import math
import pandas as pd
import numpy as np
from tqdm import tqdm
from datetime import datetime
from scipy import stats
from utils import pickle_wrap, read_csv_fast, save_csv_pkl
import unicodedata
import argparse

# ----------------- Config -----------------
STAT_LOOKBACK = 32     
STAT_LOOKAHEAD = 0 # Modify if you want to add a lookahead


def remove_diacritics_fast(text):
    return (unicodedata.normalize('NFKD', text)
            .encode('ASCII', 'ignore')
            .decode('ASCII'))

def norm_decimal_str(s):
    return s.replace('\u2009', '').replace('\xa0', '').replace(' ', '').replace(',', '.')

# ----------------- Regex "p ..." -----------------
P_TOKEN_PATTERN = re.compile(
    r'(?:^|[\s\(\[\{,;:\-])\s*'
    r'(?:p|P|p\-value|P\-value|p‐value|P‐value|p value|P value|p‐val)\s*'
    r'(?P<sign>=|<|>|≤|≥)\s*'
    r'(?P<pval>0?\.\s*\d+)',
    re.IGNORECASE
)

# ----------------- Regex effect size -----------------
_RE_FLOAT = r'-?\d+(?:[.,]\d+)?'
_RE_INT   = r'\d+'

PAT_T1 = re.compile(r't\s*\(\s*(?P<df>' + _RE_INT + r')\s*\)\s*=\s*(?P<val>' + _RE_FLOAT + r')', re.IGNORECASE)
PAT_T2 = re.compile(r't\s*=\s*(?P<val>' + _RE_FLOAT + r')\s*(?:,|\s)*(?:df|dof)\s*=?\s*(?P<df>' + _RE_INT + r')', re.IGNORECASE)
PAT_F  = re.compile(r'F\s*\(\s*(?P<df1>' + _RE_INT + r')\s*,\s*(?P<df2>' + _RE_INT + r')\s*\)\s*=\s*(?P<val>' + _RE_FLOAT + r')', re.IGNORECASE)
PAT_CHI= re.compile(
    r'(?:χ\s*\^?\s*2|χ²|chi[\-\s]*square|chi[\-\s]*squared|chi2|chisq|x\s*\^?\s*2|x²|x2)\s*'
    r'\(\s*(?P<df>' + _RE_INT + r')\s*\)\s*=\s*(?P<val>' + _RE_FLOAT + r')',
    re.IGNORECASE
)
PAT_Z  = re.compile(r'z\s*=\s*(?P<val>' + _RE_FLOAT + r')', re.IGNORECASE)
PAT_R1 = re.compile(r'r\s*\(\s*(?P<df>' + _RE_INT + r')\s*\)\s*=\s*(?P<val>-?\d[.,]\d+)', re.IGNORECASE)
PAT_R2 = re.compile(r'r\s*=\s*(?P<val>-?\d[.,]\d+)\s*(?:,|\s)+(?:n\s*=\s*(?P<n>' + _RE_INT + r'))', re.IGNORECASE)

STAT_PATTERNS = [
    ('t', PAT_T1), ('t', PAT_T2),
    ('F', PAT_F),
    ('chi2', PAT_CHI),
    ('z', PAT_Z),
    ('r', PAT_R1), ('r', PAT_R2),
]

def _to_float(x):
    try:
        return float(str(x).replace(',', '.'))
    except Exception:
        return np.nan

def extract_test_stat(window_text: str):
    if not isinstance(window_text, str) or not window_text.strip():
        return None
    matches = []
    for name, pat in STAT_PATTERNS:
        for m in pat.finditer(window_text):
            d = {'_end': m.end(), 'stat_type': name}
            gd = m.groupdict()
            if 'val' in gd and gd['val'] is not None:
                d['stat_value'] = _to_float(gd['val'])
            if 'df' in gd and gd['df'] is not None:
                d['df'] = int(gd['df'])
            if 'df1' in gd and gd['df1'] is not None:
                d['df1'] = int(gd['df1'])
            if 'df2' in gd and gd['df2'] is not None:
                d['df2'] = int(gd['df2'])
            if 'n' in gd and gd['n'] is not None:
                d['n'] = int(gd['n'])
            matches.append(d)
    if not matches:
        return None
    best = max(matches, key=lambda x: x['_end'])
    best.pop('_end', None)
    return best

def compute_p_implied_from_stat(stat: dict):
    try:
        stype = stat.get('stat_type')
        val = _to_float(stat.get('stat_value', np.nan))
        if np.isnan(val):
            return np.nan
        if stype == 't':
            df = stat.get('df', None)
            if df is None or df <= 0: return np.nan
            return float(2.0 * stats.t.sf(abs(val), int(df)))
        if stype == 'F':
            df1 = stat.get('df1', None); df2 = stat.get('df2', None)
            if not df1 or not df2 or df1 <= 0 or df2 <= 0: return np.nan
            return float(stats.f.sf(val, int(df1), int(df2)))
        if stype == 'chi2':
            df = stat.get('df', None)
            if df is None or df <= 0: return np.nan
            return float(stats.chi2.sf(val, int(df)))
        if stype == 'z':
            return float(2.0 * stats.norm.sf(abs(val)))
        if stype == 'r':
            r = val
            df = stat.get('df', None)
            if df is None:
                n = stat.get('n', None)
                if n is None or n <= 2: return np.nan
                df = n - 2
            if df <= 0 or abs(r) >= 1.0: return np.nan
            tval = r * math.sqrt(df / (1.0 - r*r))
            return float(2.0 * stats.t.sf(abs(tval), int(df)))
        return np.nan
    except Exception:
        return np.nan

# -----------------  p + p_implied extraction  -----------------
def get_sentence(tup, prev_end, stat_range, plain_text, sentence_range):
    start_idx = tup.start()
    lower_idx = max(prev_end, start_idx - stat_range)
    sentence = plain_text[lower_idx:start_idx]
    return sentence.strip()

def parse_ps(plain_text, doi_str):
    p_val_texts = list(P_TOKEN_PATTERN.finditer(plain_text))
    p_val_l = []
    prev_end = 0

    for tup in p_val_texts:
        before = get_sentence(tup, prev_end, STAT_LOOKBACK, plain_text, 512)
        sentence = remove_diacritics_fast(before) if before is not None else ""

        try:
            sign_val = tup.group('sign')
            p_val_str = tup.group('pval')
            p_val = float(norm_decimal_str(p_val_str))
        except Exception:
            prev_end = tup.end()
            continue

        start, end = tup.start(), tup.end()
        after = plain_text[end: min(len(plain_text), end + STAT_LOOKAHEAD)] if STAT_LOOKAHEAD > 0 else ""
        stat = extract_test_stat(before) or (extract_test_stat(after) if STAT_LOOKAHEAD > 0 else None)
        p_impl = compute_p_implied_from_stat(stat) if stat else np.nan

        d = {
            'sign': sign_val,
            'p_val': p_val,
            'sentence': sentence,
            'doi_str': doi_str,
            'stat_type': stat.get('stat_type') if stat else np.nan,
            'stat_value': stat.get('stat_value') if stat else np.nan,
            'df':  stat.get('df')  if stat and 'df'  in stat else np.nan,
            'df1': stat.get('df1') if stat and 'df1' in stat else np.nan,
            'df2': stat.get('df2') if stat and 'df2' in stat else np.nan,
            'n':   stat.get('n')   if stat and 'n'   in stat else np.nan,
            'p_implied': p_impl
        }
        p_val_l.append(d)
        prev_end = tup.end()

    return p_val_l

def get_paper_df(fp_text, doi_str):
    with open(fp_text, 'r', encoding='utf-8') as file:
        plain_text = file.read()
    p_val_l = parse_ps(plain_text, doi_str)
    return pd.DataFrame(p_val_l)

# ----------------- Pipeline files -> CSV -----------------
def make_p_z_df(text_dirs, save_dir):
    all_txt_files = []
    for dir_path in text_dirs:
        all_txt_files += [f for f in os.listdir(dir_path) if f.endswith('.txt')]

    file_ids = [f.replace('.txt', '') for f in all_txt_files]
    df = pd.DataFrame({'file_id': sorted(set(file_ids))})

    df_pz_as_l = []

    for row in tqdm(df.itertuples(), desc='Extracting p-values'):
        file_id = row.file_id
        doi_str_norm = file_id.replace('_', '-') 
        found = False

        for dir_path in text_dirs:
            fp_text = os.path.join(dir_path, f"{file_id}.txt")
            if os.path.exists(fp_text):
                found = True
                break

        if not found:
            continue

        fp_pkl = os.path.join("Analysis", "PTChange-main", "cache", "p_val_l", f"res_{file_id}.pkl")
        os.makedirs(os.path.dirname(fp_pkl), exist_ok=True)

        df_paper = pickle_wrap(
            get_paper_df, fp_pkl,
            kwargs={'fp_text': fp_text, 'doi_str': doi_str_norm},
            verbose=-1, easy_override=True,
            dt_max=datetime(2024, 8, 24, 10, 0, 0, 0)
        )
        df_pz_as_l.append(df_paper)

    if not df_pz_as_l:
        print("⚠️ No p-value found in the files.")
        return

    df_pz = pd.concat(df_pz_as_l, ignore_index=True)

    if 'doi_str' in df_pz.columns:
        df_pz['doi_str'] = df_pz['doi_str'].astype(str).str.replace('_', '-', regex=False)

    for col in ['stat_value','df','df1','df2','n','p_implied','p_val']:
        if col in df_pz.columns:
            df_pz[col] = pd.to_numeric(df_pz[col], errors='coerce')

    # Count and output
    num_ps = int(df_pz['p_val'].count())
    num_pimpl = int(df_pz['p_implied'].notna().sum())
    n_art = int(df_pz['doi_str'].nunique())
    n_art_pimpl = int(df_pz.groupby('doi_str')['p_implied'].apply(lambda s: s.notna().any()).sum()) if len(df_pz) else 0

    os.makedirs(save_dir, exist_ok=True)
    fp_out = os.path.join(save_dir, 'val_orig_quant_by_pval.csv')
    save_csv_pkl(df_pz, fp_out, check_dup=False)

    print(f"[OK] written -> {fp_out}")
    print(f"[STATS] p-values extracted: {num_ps} | p_implied non-NA: {num_pimpl} | "
          f"articles: {n_art} | articles with ≥1 p_implied: {n_art_pimpl}")

# ----------------- CLI -----------------
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--text_dirs', nargs='+', required=True, help='List of folders containing files .txt')
    parser.add_argument('--save_dir', type=str, required=True, help='Results backup folder')
    args = parser.parse_args()
    make_p_z_df(args.text_dirs, args.save_dir)
