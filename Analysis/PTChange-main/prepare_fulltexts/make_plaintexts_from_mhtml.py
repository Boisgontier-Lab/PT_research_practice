import os
import re
import gc
import bs4
import email
from pathlib import Path
from tqdm import tqdm
from functools import cache

# === CONFIGURATION ===
mhtml_dir = Path("../doi_tree_mhtml")
out_dir = Path("../doi_tree_mhtml_txt")
out_404_dir = out_dir / "404s"
out_dir.mkdir(parents=True, exist_ok=True)
out_404_dir.mkdir(parents=True, exist_ok=True)

@cache
def re_compile(re_str):
    return re.compile(re_str, re.IGNORECASE)

def general_parse(soup, search="h2"):
    val_str = "(result|finding)"
    results_secs_ = soup.find_all(search, string=re_compile(val_str))
    results_secs = []
    for heading in results_secs_:
        for sibling in heading.next_siblings:
            if sibling.name == search:
                break
            results_secs.append(sibling)
    return results_secs

def extract_html_from_mhtml(fp_mhtml):
    with open(fp_mhtml, 'r', encoding='utf-8', errors='ignore') as f:
        msg = email.message_from_file(f)
        for part in msg.walk():
            if part.get_content_type() == 'text/html':
                return part.get_payload(decode=True).decode('utf-8', errors='ignore')
    return None

def extract_results_from_mhtml(fp_mhtml):
    html = extract_html_from_mhtml(fp_mhtml)
    if not html:
        return None

    soup = bs4.BeautifulSoup(html, "lxml")

   # === Removing captions from figures, tables, and images ===    
    for tag in soup.find_all(['figure', 'figcaption', 'caption', 'table', 'table-wrap', 'img']):
        tag.decompose()

    for tag in soup.find_all(['p', 'div', 'span']):
        if tag.text.strip().lower().startswith(('figure', 'table')):
            tag.decompose()

    # === Search section “Results” or “Findings” ===    
    results_secs = general_parse(soup, search="h2")
    if not results_secs:
        results_secs = general_parse(soup, search="h3")

    results_texts = []
    for sec in results_secs:
        text = sec.get_text(strip=True)  
        if len(text) > 100:
            results_texts.append(text)

    return '\n\n'.join(results_texts) if results_texts else None

def make_plaintexts_from_mhtml():
    mhtml_files = sorted(mhtml_dir.glob("*.mhtml"))

    for fp in tqdm(mhtml_files, desc="📄 Extraction from MHTML"):
        doi_str = fp.stem
        fp_out = out_dir / f"{doi_str}.txt"
        fp_404 = out_404_dir / f"{doi_str}.txt"

        if fp_out.exists() or fp_404.exists():
            continue

        try:
            result_text = extract_results_from_mhtml(fp)
            if result_text:
                with open(fp_out, "w", encoding="utf-8") as f:
                    f.write(result_text)
            else:
                with open(fp_404, "w", encoding="utf-8") as f:
                    f.write("")
        except Exception as e:
            print(f"❌ Error with {fp.name} : {e}")
            with open(fp_404, "w", encoding="utf-8") as f:
                f.write("")
        gc.collect()

if __name__ == "__main__":
    make_plaintexts_from_mhtml()
