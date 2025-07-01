import json
import requests
import os
import re
import pandas as pd
import logging
from bs4 import BeautifulSoup
from urllib.parse import urljoin
from concurrent.futures import ThreadPoolExecutor, as_completed

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(BASE_DIR, 'output')
INPUT_CSV = os.path.join(BASE_DIR, 'PA_bills_to_process.csv')


def clean_html_bill_text(html_content, uuid_for_logging):
    """
    Cleans raw HTML of a bill text page by extracting text after the legislative anchor.
    """
    if not html_content:
        return ""

    soup = BeautifulSoup(html_content, 'lxml')
    pre_tags = soup.find_all('pre')  # Get all <pre> blocks
    if not pre_tags:
        return ""

    full_text = "\n".join([pre.get_text() for pre in pre_tags])

    # Use regex to find the start of the actual bill text
    anchor_regex = re.compile(
        r"The General Assembly of the Commonwealth of Pennsylvania.*?enacts as follows:",
        re.DOTALL | re.IGNORECASE
    )
    
    match = anchor_regex.search(full_text)
    start_index = 0

    if match:
        start_index = match.end()
    else:
        # Fallback anchor for resolutions
        resolved_match = re.search(r"RESOLVED,", full_text, re.IGNORECASE)
        if resolved_match:
            start_index = resolved_match.start()
        else:
            logging.warning(f"[{uuid_for_logging}] Could not find a reliable start anchor.")
            return ""

    bill_content = full_text[start_index:]

    # Remove common footer lines
    lines = bill_content.splitlines()
    cleaned_lines = []
    footer_regex = re.compile(r'^\d{5}[H|S]\d+B\d+\s+-\s*\d+\s*-$')

    for line in lines:
        stripped_line = line.strip()
        if stripped_line and not footer_regex.match(stripped_line):
            cleaned_lines.append(stripped_line)

    return "\n".join(cleaned_lines).strip()


# --- Output Helpers ---
def create_output_dirs():
    """Creates directories to save scraped output if they don't already exist."""
    logging.info("Creating output directories...")
    for subdir in ["bill_metadata", "sponsors", "text"]:
        os.makedirs(os.path.join(OUTPUT_DIR, subdir), exist_ok=True)

def write_data(file_name, directory, data):
    """Writes scraped data to the appropriate file and format."""
    file_path = os.path.join(OUTPUT_DIR, directory, file_name)
    try:
        if directory == "text":
            file_path += ".txt"
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(data)
        else:
            file_path += ".json"
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=4)
    except IOError as e:
        logging.error(f"Error saving file {file_path}: {e}")

def get_session_years(session_name_str):
    """Extracts session years (start and optionally end) from a session string."""
    match = re.search(r'(\d{4})-?(\d{4})?', session_name_str)
    if match:
        start_year, end_year = match.groups()
        return [end_year, start_year] if end_year else [start_year]
    return []


# --- Scraping Logic ---
def find_bill_page(session, state_bill_id):
    """Attempts to locate the main webpage for a bill based on session and ID."""
    session_years = get_session_years(session)
    if not session_years:
        logging.warning(f"Could not parse years from session: {session}.")
        return None, None

    formatted_bill_id = state_bill_id.replace(" ", "").lower()
    headers = {'User-Agent': 'Mozilla/5.0'}

    for year in session_years:
        url = f"https://www.palegis.us/legislation/bills/{year}/{formatted_bill_id}"
        try:
            response = requests.get(url, headers=headers, timeout=30)
            if response.status_code == 200:
                logging.info(f"Found bill page for {state_bill_id} at {url}")
                return url, BeautifulSoup(response.text, 'lxml')
        except requests.RequestException as e:
            logging.error(f"Request failed for {url}: {e}")
    logging.warning(f"Could not find bill page for {state_bill_id} in session {session}")
    return None, None

def scrape_metadata_and_sponsors(soup, uuid, session, state_bill_id, state_url):
    """Extracts metadata and sponsors from a bill’s main page."""
    if not soup:
        return

    # --- Metadata scraping ---
    try:
        title = soup.find('h1').get_text(strip=True) if soup.find('h1') else state_bill_id
        last_action_tag = soup.find(string=re.compile(r"Last Action:"))
        status = last_action_tag.find_next('div').get_text(strip=True) if last_action_tag else "Status not found"
        metadata = {
            "uuid": uuid, "state": "PA", "session": session, "state_bill_id": state_bill_id,
            "title": title, "status": status, "state_url": state_url
        }
        write_data(uuid, "bill_metadata", metadata)
    except Exception as e:
        logging.error(f"[{uuid}] Error scraping metadata: {e}")

    # --- Sponsors scraping ---
    try:
        sponsors_list = []
        text_content = soup.get_text('\n')
        lines = [line.strip() for line in text_content.splitlines() if line.strip()]
        try:
            sponsors_keyword_idx = lines.index("Sponsors")
            if sponsors_keyword_idx + 1 < len(lines):
                sponsor_line_text = lines[sponsors_keyword_idx + 1]
                cleaned_sponsor_line = sponsor_line_text.replace(u'\u00a0', ' ')
                sponsor_names = [name.strip().title() for name in cleaned_sponsor_line.split(',') if name.strip()]
                sponsors_list = [{"sponsor_name": name} for name in sponsor_names]
        except ValueError:
            logging.warning(f"[{uuid}] Could not find 'Sponsors' keyword.")

        sponsor_data = {
            "uuid": uuid, "state": "PA", "session": session,
            "state_bill_id": state_bill_id, "sponsors": sponsors_list
        }
        write_data(uuid, "sponsors", sponsor_data)
    except Exception as e:
        logging.error(f"[{uuid}] Critical error scraping sponsors: {e}")

def scrape_text(soup, uuid, state_url):
    """Finds and processes the bill's text from the HTML Bill Text page."""
    if not soup:
        return

    # Try to find a link to the bill's HTML text
    bill_text_link_tag = soup.find('a', attrs={'data-bs-original-title': 'HTML Bill Text'})
    if not bill_text_link_tag:
        bill_text_link_tag = soup.find('a', href=lambda href: href and "/legislation/bills/text/HTM/" in href)

    if bill_text_link_tag and bill_text_link_tag.get('href'):
        text_url_relative = bill_text_link_tag.get('href')
        text_url_absolute = urljoin(state_url, text_url_relative)

        try:
            response = requests.get(text_url_absolute, headers={'User-Agent': 'Mozilla/5.0'}, timeout=30)
            response.raise_for_status()
            cleaned_text = clean_html_bill_text(response.text, uuid)
            write_data(uuid, "text", cleaned_text)
        except requests.RequestException as e:
            logging.error(f"[{uuid}] Failed to fetch bill text: {e}")
    else:
        logging.warning(f"[{uuid}] No HTML Bill Text link found.")

def scrape_bill_entrypoint(uuid, session, state_bill_id):
    """Wrapper to scrape a single bill using its UUID, session, and ID."""
    logging.info(f"--- Starting scrape for {uuid} ({state_bill_id}) ---")
    state_url, soup = find_bill_page(session, state_bill_id)
    if not soup or not state_url:
        logging.error(f"[{uuid}] Could not retrieve bill page.")
        return
    scrape_metadata_and_sponsors(soup, uuid, session, state_bill_id, state_url)
    scrape_text(soup, uuid, state_url)


# --- Main Script Execution ---
if __name__ == "__main__":
    create_output_dirs()

    # Check for the input CSV
    if not os.path.exists(INPUT_CSV):
        logging.critical(f"Input file not found at {INPUT_CSV}. Please create it.")
        exit()

    # Load list of bills to scrape
    bill_list = pd.read_csv(INPUT_CSV)

    # Avoid re-scraping bills already processed
    metadata_dir = os.path.join(OUTPUT_DIR, "bill_metadata")
    existing_uuids = {filename.removesuffix(".json") for filename in os.listdir(metadata_dir)}
    bill_list = bill_list[~bill_list["UUID"].isin(existing_uuids)]

    if bill_list.empty:
        logging.info("All bills have already been processed.")
    else:
        logging.info(f"Found {len(bill_list)} new bills to process.")
        bill_rows = bill_list[["UUID", "session", "bill_number"]].to_dict(orient="records")

        # Parallel scraping using threads
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = {
                executor.submit(scrape_bill_entrypoint, row["UUID"], row["session"], row["bill_number"]): row
                for row in bill_rows
            }
            for future in as_completed(futures):
                row = futures[future]
                try:
                    future.result()
                    logging.info(f"--- Finished scrape for {row['UUID']} ---")
                except Exception as exc:
                    logging.error(f"Bill {row} generated an exception: {exc}", exc_info=True)

    logging.info("Scraping process complete.")
