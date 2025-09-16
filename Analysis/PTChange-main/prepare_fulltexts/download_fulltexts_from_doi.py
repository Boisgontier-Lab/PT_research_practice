import os
import time
import random
import pandas as pd
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options

# === PATHS===
PATH_TO_CHROMEDRIVER = r"C:\..." # ENTER THE PATH TO YOUR CHROMEDRIVER.EXE FILE HERE
CSV_PATH = r"../dataframes/val_orig_quant.csv"
OUT_DIR = Path("../doi_tree_html")
OUT_DIR.mkdir(parents=True, exist_ok=True)

# === LOAD DOIS ===
df = pd.read_csv(CSV_PATH)
df = df.dropna(subset=["doi"])
df["doi"] = df["doi"].apply(lambda x: str(x).strip().replace("https://doi.org/", "").replace("http://doi.org/", "").split(";")[0].split(" ")[0])
print(f"🔎 {len(df)} DOIs to be processed")

# === MAIN LOOP ===
for idx, doi in enumerate(df["doi"]):
    url = f"https://doi.org/{doi}"
    doi_str = doi.replace("/", "_").replace(".", "-").replace("(", "_").replace(")", "_")
    fp_out = OUT_DIR / f"{doi_str}.html"
    fp_404 = OUT_DIR / "404s" / f"{doi_str}"

    if fp_out.exists() or fp_404.exists():
        continue

    print(f"\n🔗 [{idx+1}] Processing of : {url}")

    # === NEW CHROME SESSION ===
    chrome_options = Options()
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--window-size=1920,1080")
    chrome_options.add_argument("--disable-dev-shm-usage")
    chrome_options.add_argument("--no-sandbox")
    # chrome_options.add_argument("--headless")  
    chrome_options.add_argument("--disable-blink-features=AutomationControlled")
    
    service = Service(PATH_TO_CHROMEDRIVER)
    driver = webdriver.Chrome(service=service, options=chrome_options)

    try:
        driver.get(url)
        print("⏳ Waiting for manual resolution of Cloudflare...")

        max_wait_time = 300  # 5 min max
        check_interval = 5
        waited = 0

        while True:
            time.sleep(check_interval)
            waited += check_interval
            html = driver.page_source.lower()

            if "just a moment" in html or "cloudflare" in html or len(html.strip()) < 1000:
                print(f"⌛ Pending... ({waited}s)")
                if waited >= max_wait_time:
                    print(f"❌ Timeout Cloudflare for {doi}. Marked as 404.")
                    Path(fp_404.parent).mkdir(parents=True, exist_ok=True)
                    with open(fp_404, "w", encoding="utf-8") as f:
                        f.write("")
                    break
            else:
                html_full = driver.page_source
                with open(fp_out, "w", encoding="utf-8") as f:
                    f.write(html_full)
                print(f"✅ Saved in: {fp_out}")
                break

    except Exception as e:
        print(f"❌ Error with {doi} : {e}")
        try:
            Path(fp_404.parent).mkdir(parents=True, exist_ok=True)
            with open(fp_404, "w", encoding="utf-8") as f:
                f.write("")
        except:
            pass

    finally:
        driver.quit()
        print("🧹 Chrome closed.")
        sleep_duration = random.uniform(8, 14)
        print(f"⏸ {sleep_duration:.1f} secondes break before the next DOI")
        time.sleep(sleep_duration)

print("🏁 Finished.")
