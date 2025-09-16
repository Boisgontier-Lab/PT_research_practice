# download_fulltexts_pmcid.py

import os
import time
import pandas as pd
from pathlib import Path
import requests

# === PATHS ===
CSV_PATH = r"../dataframes/pmcid.csv"
OUT_DIR = Path("../doi_tree_html")
OUT_DIR.mkdir(parents=True, exist_ok=True)
OUT_404 = OUT_DIR / "404s"
OUT_404.mkdir(parents=True, exist_ok=True)

# === LOAD PMCIDs ===
df = pd.read_csv(CSV_PATH)
df = df.dropna(subset=["pmcid", "doi"])
print(f"🔎 {len(df)} PMCIDs to be processed")

# === DOWNLOADING ===
for idx, row in enumerate(df.itertuples(), 1):
    pmcid = row.pmcid
    doi = row.doi
    doi_str = doi.replace("/", "_").replace(".", "-").replace("(", "_").replace(")", "_")
    fp_out = OUT_DIR / f"{doi_str}.html"
    fp_404 = OUT_404 / f"{doi_str}"

    if fp_out.exists() or fp_404.exists():
        continue

    print(f"\n🔗 [{idx}] Downloading : {pmcid}")

    url = f"https://www.ncbi.nlm.nih.gov/pmc/articles/{pmcid}/"
    headers = {
        "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119 Safari/537.36"
    }

    try:
        response = requests.get(url, headers=headers, timeout=15)

        if response.status_code == 200 and len(response.text) > 1000:
            with open(fp_out, "w", encoding="utf-8") as f:
                f.write(response.text)
            print(f"✅ Saved in: {fp_out}")
        else:
            with open(fp_404, "w", encoding="utf-8") as f:
                f.write("")
            print(f"❌ Failed or empty → 404 for {pmcid}")

    except Exception as e:
        print(f"❌ Error with {pmcid} : {e}")
        with open(fp_404, "w", encoding="utf-8") as f:
            f.write("")

    time.sleep(0.5)  

print("🏁 Finished.")
