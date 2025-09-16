import pandas as pd
import time
import pyautogui
import subprocess
import os
import shutil

# === CONFIGURATION ===
CSV_PATH = "../dataframes/pii.csv"
CHROME_PATH = r"C:\..." # ENTER THE PATH TO YOUR CHROME.EXE FILE
WAIT_BEFORE_SAVE = 10
WAIT_AFTER_TYPING = 1
WAIT_AFTER_SAVE = 11
TEMP_PROFILE_BASE = "C:/temp_chrome_profiles"

def clean_filename(doi):
    return doi.replace("/", "-").replace(".", "-").replace("(", "-").replace(")", "-")

# Read data
df = pd.read_csv(CSV_PATH).dropna(subset=["pii", "doi"])
os.makedirs(TEMP_PROFILE_BASE, exist_ok=True)

for idx, row in df.iterrows():
    pii = row["pii"]
    doi = row["doi"]
    filename = clean_filename(doi)
    url = f"https://www.sciencedirect.com/science/article/pii/{pii}"
    profile_dir = os.path.join(TEMP_PROFILE_BASE, f"profile_{idx}")

    print(f"\n[{idx+1}/{len(df)}] 🌐 Access to: {url}")

    # Launch chrome with a proper profile
    chrome_proc = subprocess.Popen([
        CHROME_PATH,
        "--new-window",
        f"--user-data-dir={profile_dir}",
        "--no-first-run",
        "--disable-first-run-ui",
        "--no-default-browser-check",
        url
    ])

    # Waiting for page to load
    time.sleep(WAIT_BEFORE_SAVE)

    print("💾 Ctrl+S to initiate storage...")
    pyautogui.hotkey("ctrl", "s")
    time.sleep(2)

    print(f"📝 Writing file name: {filename}")
    pyautogui.hotkey("ctrl", "a")
    time.sleep(0.3)
    pyautogui.press("backspace")
    time.sleep(0.3)
    pyautogui.write(filename)
    time.sleep(WAIT_AFTER_TYPING)

    print("📂 Single file format selection...")
    pyautogui.press("tab", presses=1, interval=0.3)
    pyautogui.press("down")
    pyautogui.press("up")  
    pyautogui.press("enter")
    pyautogui.press("enter")

    print("📥 Save...")
    time.sleep(WAIT_AFTER_SAVE)

    print("❌ Closing Chrome...")
    chrome_proc.terminate()
    time.sleep(2)

    # Clean temporary profile
    try:
        shutil.rmtree(profile_dir)
    except Exception as e:
        print(f"⚠️ Profile deletion error : {e}")

    print("✅ Finished for this article.")

print("\n🏁 All articles have been processed.")
