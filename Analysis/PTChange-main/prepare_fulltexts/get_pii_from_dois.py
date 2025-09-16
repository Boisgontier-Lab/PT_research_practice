import pandas as pd
import requests
import time
from pathlib import Path

# === CONFIGURATION ===
API_KEY = "..."  # ← TO REPLACE BY YOUR ELSEVIER KEY (it's free)
INPUT_PATH = Path("../dataframes/val_missing2.csv")
OUTPUT_PATH = Path("../dataframes/pii.csv")

# === LOADING DOIs ===
df = pd.read_csv(INPUT_PATH)
df["doi"] = df["doi"].astype(str).str.strip()
df = df.dropna(subset=["doi"])
df = df[df["doi"] != ""]

# === FUNCTION FOR QUERYING THE ELSEVIER API ===
def get_pii_from_doi(doi):
    url = f"https://api.elsevier.com/content/article/doi/{doi}"
    headers = {
        "Accept": "application/json",
        "X-ELS-APIKey": API_KEY
    }
    try:
        r = requests.get(url, headers=headers, timeout=10)
        if r.status_code == 200:
            data = r.json()
            pii = data.get("full-text-retrieval-response", {}).get("coredata", {}).get("pii")
            return pii
    except Exception as e:
        print(f"❌ Error with {doi} : {e}")
    return None

# === MAIN LOOP ===
results = []
for i, doi in enumerate(df["doi"], 1):
    print(f"🔎 [{i}/{len(df)}] DOI: {doi}")
    pii = get_pii_from_doi(doi)
    if pii:
        print(f"✅ PII found : {pii}")
        results.append({"doi": doi, "pii": pii})
    else:
        print("⚠️ No PII found")
    time.sleep(0.35)  

# === SAVE FINAL CSV ===
df_results = pd.DataFrame(results)
df_results.to_csv(OUTPUT_PATH, index=False)
print(f"\n💾 File saved : {OUTPUT_PATH.resolve()}")
print(f"✅ {len(df_results)} PIIs found for {len(df)} DOIs")
