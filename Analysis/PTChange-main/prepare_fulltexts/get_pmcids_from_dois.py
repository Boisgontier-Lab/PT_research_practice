import pandas as pd
import requests
import time
from xml.etree import ElementTree as ET
from pathlib import Path

# === CONFIGURATION ===
EMAIL = ""  # TO REPLACE BY YOUR EMAIL (mandatory for the NCBI API)
INPUT_PATH = Path("../dataframes/val_missing1.csv")  
OUTPUT_PATH = Path("../dataframes/pmcid.csv")

# === LOADING DOIs ===
df = pd.read_csv(INPUT_PATH)
df["doi"] = df["doi"].astype(str).str.strip()
df = df.dropna(subset=["doi"])
df = df[df["doi"] != ""]

# === FUNCTION FOR QUERYING THE NCBI API ===
def get_pmcid_from_doi(doi):
    base_url = "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
    params = {
        "tool": "PsychChangeScript",
        "email": EMAIL,
        "ids": doi,
        "format": "xml"
    }
    try:
        r = requests.get(base_url, params=params, timeout=10)
        if r.status_code == 200:
            root = ET.fromstring(r.content)
            record = root.find("record")
            if record is not None and "pmcid" in record.attrib:
                return record.attrib["pmcid"]
    except Exception as e:
        print(f"❌ Error with {doi} : {e}")
    return None

# === MAIN LOOP ===
results = []
for i, doi in enumerate(df["doi"], 1):
    print(f"🔎 [{i}/{len(df)}] DOI: {doi}")
    pmcid = get_pmcid_from_doi(doi)
    if pmcid:
        print(f"✅ PMCID found: {pmcid}")
        results.append({"doi": doi, "pmcid": pmcid})
    else:
        print("⚠️ No PMCID found")
    time.sleep(0.34)  

# === SAVED FINAL CSV ===
df_results = pd.DataFrame(results)
df_results.to_csv(OUTPUT_PATH, index=False)
print(f"\n💾 File saved: {OUTPUT_PATH.resolve()}")
print(f"✅ {len(df_results)} PMCIDs found for {len(df)} DOIs")
