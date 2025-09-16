import gc
import os
import re
from pathlib import Path
from functools import cache
import bs4
from tqdm import tqdm

# === CONFIGURATION ===
html_dir = Path('../doi_tree_html')
out_dir = Path('../doi_tree_txt')
out_404_dir = out_dir / '404s'
out_dir.mkdir(parents=True, exist_ok=True)
out_404_dir.mkdir(parents=True, exist_ok=True)


@cache
def re_compile(re_str):
    return re.compile(re_str, re.IGNORECASE)


def general_parse(soup, search='h2'):
    val_str = '(result|finding)'
    results_secs_ = soup.find_all(search, string=re_compile(val_str))
    if len(results_secs_) > 0:
        results_secs = []
        for heading in results_secs_:
            for sibling in heading.next_siblings:
                if sibling.name == search:
                    break
                results_secs.append(sibling)
        return results_secs
    return []


def extract_results_from_html(fp_html):
    with open(fp_html, 'r', encoding='utf-8') as f:
        html = f.read()

    soup = bs4.BeautifulSoup(html, 'lxml')

    # === Removing captions from figures, tables, and images ===
    for tag in soup.find_all(['figure', 'figcaption', 'caption', 'table', 'table-wrap', 'img']):
        tag.decompose()

    for tag in soup.find_all(['p', 'div', 'span']):
        if tag.text.strip().lower().startswith(('figure', 'table')):
            tag.decompose()

    # === Search section “Results” or “Findings” ===
    results_secs = general_parse(soup, search='h2')

    if len(results_secs) == 0:
        results_secs = general_parse(soup, search='h3')

    # === Final clean up ===
    results_texts = []
    for sec in results_secs:
        text = sec.get_text(strip=True)
        if len(text) > 100:
            results_texts.append(text)

    if not results_texts:
        return None

    result_text = '\n\n'.join(results_texts)
    return result_text


def make_results_only_plaintexts():
    html_files = sorted(html_dir.glob("*.html"))

    for fp_html in tqdm(html_files, desc="Results extraction"):
        doi_str = fp_html.stem
        fp_out = out_dir / f"{doi_str}.txt"
        fp_404 = out_404_dir / f"{doi_str}.txt"

        if fp_out.exists() or fp_404.exists():
            continue

        try:
            result_text = extract_results_from_html(fp_html)
            if result_text:
                with open(fp_out, 'w', encoding='utf-8') as f:
                    f.write(result_text)
            else:
                with open(fp_404, 'w', encoding='utf-8') as f:
                    f.write('')
        except Exception as e:
            print(f"❌ Error with {doi_str} : {e}")
            with open(fp_404, 'w', encoding='utf-8') as f:
                f.write('')
        gc.collect()


if __name__ == '__main__':
    make_results_only_plaintexts()
