from collections import defaultdict
from datetime import datetime
import os

import numpy as np
import pandas as pd
from tqdm import tqdm

from utils import read_csv_fast, pickle_wrap, save_csv_pkl

smol = 1e-6
pd.options.mode.chained_assignment = None


base_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
data_dir = os.path.join(base_dir, "dataframes")
cache_dir = os.path.join(base_dir, "cache")

# Input/output
fp_p_by_p = os.path.join(data_dir, "val_orig_quant_by_pval.csv")
fp_semi_prep = os.path.join(cache_dir, "val_orig_semi_prep_p.pkl")
fp_out_detail = os.path.join(data_dir, "val_orig_quant_processed_by_p.csv")
fp_out_summary = os.path.join(data_dir, "val_orig_quant_process.csv")


def categorize_paper_pvals(df_by_p, thresh=.999999, p_key=''):
    doi2cond = {}
    doi2lower_cutoff = {}

    if p_key is None:
        p_key = 'p_val'
    else:
        p_key = f'p_val{p_key}'

    df_p_by_p_ = df_by_p[['doi_str', 'sign', p_key]]
    for doi_str, df_doi in tqdm(df_p_by_p_.groupby('doi_str'), desc='categorizing papers'):
        doi2cond[doi_str] = 'ERROR'

        is_sig = ((df_doi[p_key] < .05 + smol) & (df_doi['sign'].isin(['<', '='])))
        df_sig = df_doi[is_sig]
        if not len(df_sig):
            doi2cond[doi_str] = 'no_sig'
            doi2lower_cutoff[doi_str] = pd.NA
            continue

        equal05 = ((df_sig[p_key] < .05 + smol) & (df_sig[p_key] > .05 - smol) & (df_sig['sign'] == '='))
        df_sig = df_sig[~equal05]

        if not len(df_sig):
            doi2cond[doi_str] = 'all_equal0.05'
            doi2lower_cutoff[doi_str] = .05
            continue

        prop_less_than = (df_sig['sign'] == '<').mean()
        if prop_less_than > thresh:
            for cutoff_ in [.000, .0001, .0005, .001, .005, .01, .05, '_custom']:
                if cutoff_ == '_custom':
                    cutoff = df_sig[p_key].mode().iloc[0]
                else:
                    cutoff = cutoff_

                is_less_cut = ((df_sig[p_key] < cutoff + smol) &
                               (df_sig[p_key] > cutoff - smol) &
                               (df_sig['sign'] == '<'))
                if is_less_cut.mean() > thresh:
                    doi2cond[doi_str] = f'all_less{cutoff_}'
                    doi2lower_cutoff[doi_str] = cutoff
                    break
            else:
                lowest_p = df_sig[p_key].min()
                for cutoff_ in [.000, .0001, .0005, .001, .005, .01]:
                    if cutoff_ - smol < lowest_p < cutoff_ + smol:
                        doi2cond[doi_str] = f'all_less_mixed{cutoff_}'
                        doi2lower_cutoff[doi_str] = cutoff_
                        break
                else:
                    doi2cond[doi_str] = 'all_less_mixed_custom'
                    doi2lower_cutoff[doi_str] = lowest_p
            continue

        if (df_sig['sign'] == '=').mean() > thresh:
            doi2cond[doi_str] = 'all_equal'
            doi2lower_cutoff[doi_str] = df_sig[p_key].min()
            continue

        for cutoff_ in [.000, .0001, .0005, .001, .005, .01, '_custom']:
            if cutoff_ == '_custom':
                cutoff = df_sig[df_sig['sign'] == '<'][p_key].mode().iloc[0]
            else:
                cutoff = cutoff_

            is_less_cut = ((df_sig[p_key] < cutoff + smol) &
                           (df_sig[p_key] > cutoff - smol) &
                           (df_sig['sign'] == '<'))
            is_equal_cut = ((df_sig['sign'] == '=') & (df_sig[p_key] > cutoff - smol))
            prop = (is_less_cut | is_equal_cut).mean()
            if prop > thresh:
                doi2cond[doi_str] = f'equal_less{cutoff_}'
                doi2lower_cutoff[doi_str] = cutoff
                break
        else:
            doi2cond[doi_str] = 'eclectic'
            doi2lower_cutoff[doi_str] = pd.NA

    return df_by_p, doi2cond, doi2lower_cutoff


def semi_prep(key=None):
    df_by_p = read_csv_fast(fp_p_by_p, check_dup=False)

    if 'lens_id' in df_by_p.columns and 'doi_str' not in df_by_p.columns:
        df_by_p.rename(columns={'lens_id': 'doi_str'}, inplace=True)

    sign_mapper = {'=': '=', '<': '<', '>': '>', '≤': '<', '≥': '>'}
    df_by_p['sign'] = df_by_p['sign'].map(sign_mapper)

    return categorize_paper_pvals(df_by_p, p_key=key)


def parse_p(stat, sign):
    out = {
        'sig_exact': 0, 'n05_exact': 0, 'n005_h_exact': 0, 'n005_l_exact': 0,
        'n001_exact': 0, 'num_ps_exact': 0,
        'sig_less': 0, 'n05_less': 0, 'n005_h_less': 0, 'n005_l_less': 0,
        'n001_less': 0, 'num_ps_less': 0,
        'insig_exact': 0, 'insig_less': 0, 'insig_over': 0,
        'num_ps_any': 0, 'n_exact05': 0
    }

    if not pd.isna(stat):
        if sign == '=':
            if 0.05 - smol < stat < .05 + smol:
                out['n_exact05'] = 1
            elif stat < .05 + smol:
                out['num_ps_exact'] = 1
                out['sig_exact'] = 1
                if stat > .01 - smol:
                    out['n05_exact'] = 1
                elif stat > .005 - smol:
                    out['n005_h_exact'] = 1
                elif stat > .001 - smol:
                    out['n005_l_exact'] = 1
                else:
                    out['n001_exact'] = 1
            else:
                out['num_ps_exact'] = 1
                out['insig_exact'] = 1
        elif sign == '<':
            out['num_ps_less'] = 1
            if stat < .05 + smol:
                out['sig_less'] = 1
                if stat > .01 + smol:
                    out['n05_less'] = 1
                elif stat > .005 + smol:
                    out['n005_h_less'] = 1
                elif stat > .001 + smol:
                    out['n005_l_less'] = 1
                else:
                    out['n001_less'] = 1
                    out['n001_exact'] = 1
            else:
                out['insig_less'] = 1
        elif sign == '>':
            out['insig_over'] = 1
        else:
            raise ValueError

    out['sig'] = out['sig_exact'] or out['sig_less']
    out['n05'] = out['n05_exact'] or out['n05_less']
    out['n005_h'] = out['n005_h_exact'] or out['n005_h_less']
    out['n005_l'] = out['n005_l_exact'] or out['n005_l_less']
    out['n01'] = out['n005_h'] or out['n005_l']
    out['n001'] = out['n001_exact'] or out['n001_less']
    out['n01_001'] = out['n01'] or out['n001']
    out['num_ps'] = out['num_ps_exact'] or out['num_ps_less']
    out['num_ps_any'] = out['num_ps'] or out['insig_over'] or out['n_exact05']
    out['insig'] = out['insig_exact'] or out['insig_less'] or out['insig_over']
    out['sig_exact'] = out['sig_exact'] or out['n001_less']
    out['num_ps_exact'] = out['num_ps_exact'] or out['n001_less']

    return out


def make_p_processed_dfs():
    df_by_p, doi2cond, doi2lower_cutoff = pickle_wrap(
        semi_prep, fp_semi_prep,
        RAM_cache=False, easy_override=True,
        dt_max=datetime.now()
    )

    p_cats = df_by_p[['p_val', 'sign']].apply(lambda x: parse_p(x['p_val'], x['sign']), axis=1)
    df_by_p = pd.concat([df_by_p, pd.DataFrame(p_cats.tolist())], axis=1)

    df_by_p['cnt'] = 1
    df_by_p['cond'] = df_by_p['doi_str'].map(doi2cond)

    keys = list(p_cats.iloc[0].keys())
    agg = df_by_p.groupby('doi_str', as_index=False)[keys + ['cnt']].sum()

    lowest = (
        df_by_p.groupby('doi_str', as_index=False)['p_val']
        .min()
        .rename(columns={'p_val': 'lowest_p_val'})
    )

    df = (
        agg.merge(lowest, on='doi_str', how='left')
           .assign(
               cond=lambda d: d['doi_str'].map(doi2cond),
               p_cutoff=lambda d: d['doi_str'].map(doi2lower_cutoff)
           )
    )

    df['p_fragile'] = np.where(df['sig'] > 0, df['n05'] / df['sig'], np.nan)
    df['p_fragile_w_exact05'] = np.where(
        (df['sig'] + df['n_exact05']) > 0,
        (df['n05'] + df['n_exact05']) / (df['sig'] + df['n_exact05']),
        np.nan
    )
    df['p_fragile_orig'] = df['p_fragile']
    df.loc[df['cond'] == 'all_less0.05', 'p_fragile'] = df['p_fragile'].fillna(0.509)

    save_csv_pkl(df_by_p, fp_out_detail, check_dup=False)
    save_csv_pkl(df, fp_out_summary, check_dup=False)

    return df


def _label_fragile_implied(p_imp):
    if pd.isna(p_imp):
        return pd.NA
    x = float(p_imp)
    if x > 0.05:
        return "INSIG"
    if x < 0.01:
        return "STRONG"
    return "FRAGILE"

def _fragile_reported(sign, pv):
    if pd.isna(sign) or pd.isna(pv):
        return pd.NA
    s = str(sign)
    x = float(pv)
    if s == '=':
        return (0.01 <= x < 0.05)
    if s == '<':
        if x <= 0.01:
            return False
        if x <= 0.05:
            return True
        return pd.NA
    return pd.NA

def _safe_div(num, den):
    return (num / den) if (den and den != 0 and not pd.isna(den)) else np.nan

def _compute_implied_ratio_per_doi(df_by_p):
    if 'p_implied' not in df_by_p.columns:
        return pd.Series(dtype=float)

    work = df_by_p[['doi_str', 'p_implied']].copy()
    work['sig_imp']  = (work['p_implied'] <= 0.05).astype('Int64')
    work['frag_imp'] = ((work['p_implied'] >= 0.01) & (work['p_implied'] <= 0.05)).astype('Int64')
    grp = work.groupby('doi_str', as_index=True)[['sig_imp','frag_imp']].sum(min_count=1)
    ratio = grp.apply(lambda r: _safe_div(r['frag_imp'], r['sig_imp']), axis=1)
    ratio.name = 'p_fragile_imp_implied'
    return ratio

def patch_outputs_with_implied(fallback_allless=0.51, write_back=True):
    # -------- details by p --------
    by_p = read_csv_fast(fp_out_detail, check_dup=False)
    for c in ['p_implied', 'sign', 'p_val']:
        if c not in by_p.columns:
            by_p[c] = pd.NA

    by_p['p_is_fragile_reported'] = by_p[['sign','p_val']].apply(
        lambda r: _fragile_reported(r['sign'], r['p_val']), axis=1
    )
    by_p['p_is_fragile_implied'] = by_p['p_implied'].apply(_label_fragile_implied)
    by_p['ambig_less05'] = (by_p['sign'] == '<') & (by_p['p_val'].sub(0.05).abs() < smol)

    # -------- summary by article --------
    paper = read_csv_fast(fp_out_summary, check_dup=False)
    if 'doi_str' not in paper.columns:
        paper = paper.reset_index().rename(columns={'index':'doi_str'})

    impl_ratio = _compute_implied_ratio_per_doi(by_p)
    paper = paper.merge(impl_ratio.reset_index(), on='doi_str', how='left')

    src = 'p_fragile_orig' if 'p_fragile_orig' in paper.columns else 'p_fragile'
    paper['p_fragile_prop_raw'] = paper[src]

    # final logic: implied if available; otherwise 0.51 for all_less0.05 
    mask_allless = (paper.get('cond') == 'all_less0.05')
    use_impl = mask_allless & paper['p_fragile_imp_implied'].notna()
    use_51   = mask_allless & paper['p_fragile_imp_implied'].isna()

    paper['p_fragile_prop_adj'] = paper['p_fragile_prop_raw']
    paper.loc[use_impl, 'p_fragile_prop_adj'] = paper.loc[use_impl, 'p_fragile_imp_implied']
    paper.loc[use_51,   'p_fragile_prop_adj'] = float(fallback_allless)
    paper['p_fragile_prop_adj'] = paper['p_fragile_prop_adj'].clip(0,1)

    paper['fragile_implied_applied'] = use_impl
    paper['fragile_51_applied'] = use_51

    if write_back:
        save_csv_pkl(by_p, fp_out_detail, check_dup=False)
        save_csv_pkl(paper, fp_out_summary, check_dup=False)

    # ---- Recap ----
    n_all_less = int(mask_allless.sum())
    n_implied  = int(use_impl.sum())
    n_051      = int(use_51.sum())

    print("\n[RECAP CORRECTION]")
    print(f" - Articles with 'p < .05' only (all_less0.05) : {n_all_less}")
    print(f" - … corrected with p_implied                   : {n_implied}")
    print(f" - … corrected with 0.51                : {n_051}\n")

    return by_p, paper


# ======================== MAIN ========================
if __name__ == '__main__':
    _ = make_p_processed_dfs()
    patch_outputs_with_implied(fallback_allless=0.51, write_back=True)
