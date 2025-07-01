import os
import re
import requests
import pandas as pd
from bs4 import BeautifulSoup
from concurrent.futures import ProcessPoolExecutor
from tqdm import tqdm
import warnings
import time
import random

MAX_WORKERS = 3

# CHANGE PATHS IF ADAPTING

MASTER_FILE_PATH = '/Users/justin/Desktop/GitHub.nosync/State_Legislation/text-scrapers/vrleg_master_file.csv'
TEXT_OUTPUT_PATH = '/Users/justin/Desktop/GitHub.nosync/State_Legislation/text-scrapers/outputs/NH'


def clean_html_nh(html_content: str) -> str:
    """
    Cleans raw HTML from the NH bill text page. This version is more robust
    and does not depend on finding a specific <pre> tag.

    Args:
        html_content: A string of HTML from a bill text page.

    Returns:
        A cleaned string containing only the core text and amendment tags.
    """
    text_content = html_content

    text_content = re.sub(r'\[<s>(.*?)</s>\]', r'<strike class="amendmentDeletedText">\1</strike>', text_content, flags=re.DOTALL)
    
    text_content = re.sub(r'<u>(.*?)</u>', r'<u class="amendmentInsertedText">\1</u>', text_content, flags=re.DOTALL)

    placeholders = {
        '<u class="amendmentInsertedText">': '~~~INSERT_START~~~',
        '</u>': '~~~INSERT_END~~~',
        '<strike class="amendmentDeletedText">': '~~~DELETE_START~~~',
        '</strike>': '~~~DELETE_END~~~'
    }
    for tag, placeholder in placeholders.items():
        text_content = text_content.replace(tag, placeholder)

    text_content = re.sub(r'<[^>]+>', ' ', text_content)

    for tag, placeholder in placeholders.items():
        text_content = text_content.replace(placeholder, tag)

    cleaned_text = ' '.join(text_content.split())
    
    return cleaned_text.strip()


def build_text_url_nh(session_year: str, bill_id: str) -> str:
    """
    Constructs the direct URL for a bill's HTML text page.

    Args:
        session_year: The legislative year (e.g., "2001").
        bill_id: The formatted bill identifier, like "HB0503".

    Returns:
        The full URL to the bill text HTML file.
    """
    return f"https://gc.nh.gov/legislation/{session_year}/{bill_id}.html"


def scrape_and_save_nh(args: tuple):
    """
    Worker function to download bill text directly from a constructed URL.
    This version includes a delay to avoid rate-limiting.
    """
    uuid, session_year, formatted_bill_id = args
    if not formatted_bill_id:
        print(f"Skipping {uuid} due to invalid bill format.")
        return
        
    print(f"Processing: {uuid}")
    
    text_url = build_text_url_nh(session_year, formatted_bill_id)
    try:
        print(f"  -> Downloading from direct URL: {text_url}")
        
        output_dir = os.path.join(TEXT_OUTPUT_PATH, uuid)
        os.makedirs(output_dir, exist_ok=True)
        
        text_response = requests.get(text_url, timeout=20) # Increased timeout slightly
        text_response.raise_for_status()

        raw_html = text_response.text
        if raw_html:
            cleaned_text = clean_html_nh(raw_html)
            output_path = os.path.join(output_dir, f"{formatted_bill_id}.txt")
            
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(cleaned_text)

    except requests.exceptions.RequestException as e:
        print(f"Error fetching text for {uuid} from {text_url}: {e}")
    except Exception as e:
        print(f"An unexpected error occurred while processing {uuid}: {e}")
    finally:
        # MODIFICATION 2: Add a random pause after each attempt (success or fail)
        # to be respectful to the server and avoid getting blocked.
        pause_duration = random.uniform(30, 35) # Pause for 1 to 4 seconds
        # print(f"  -> Pausing for {pause_duration:.2f} seconds...")
        time.sleep(pause_duration)


def main():
    """
    Main execution function: loads the master list of bills, filters it for NH,
    and starts the parallel scraping process.
    """
    warnings.filterwarnings("ignore", category=requests.packages.urllib3.exceptions.InsecureRequestWarning)
    
    print("Loading and preparing master file...")
    try:
        master_df = pd.read_csv(MASTER_FILE_PATH)
    except FileNotFoundError:
        print(f"Error: Master file not found at '{MASTER_FILE_PATH}'.")
        print("Please ensure you have converted the .rds file to .csv and placed it in the correct directory.")
        return

    print("Filtering bills for NH, years 2001-2014...")
    df = master_df[
        (master_df['STATE'] == 'NH') &
        (master_df['YEAR'].between(2001, 2014))
    ].copy()

    def format_bill_id_for_direct_url(bill_part):
        if pd.isna(bill_part):
            return None
        
        match = re.match(r'([A-Z]+)(\d+)', bill_part.upper())
        if not match:
            return None
        
        bill_prefix = match.group(1)
        bill_number_str = match.group(2)
        
        if bill_prefix == 'H':
            bill_prefix = 'HB'
        elif bill_prefix == 'S':
            bill_prefix = 'SB'

        padded_number = bill_number_str.zfill(4)
        
        return f"{bill_prefix}{padded_number}"

    df['formatted_bill_id'] = df['UUID'].str.extract(r'NH\d{4}(.*)')[0].apply(format_bill_id_for_direct_url)
    df['session_year'] = df['YEAR'].astype(int).astype(str)
    
    bills_to_scrape = df[['UUID', 'session_year', 'formatted_bill_id']].dropna()
    
    scrape_args = list(bills_to_scrape.itertuples(index=False, name=None))
    
    print(f"Found {len(scrape_args)} bills to scrape for New Hampshire (2001-2014).")
    print(f"Starting scrape with {MAX_WORKERS} parallel workers and randomized delays...")
    
    with ProcessPoolExecutor(max_workers=MAX_WORKERS) as executor:
        list(tqdm(executor.map(scrape_and_save_nh, scrape_args), total=len(scrape_args)))

    print("Scraping complete.")


if __name__ == "__main__":
    main()