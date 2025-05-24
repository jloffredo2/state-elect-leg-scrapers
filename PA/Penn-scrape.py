import json
import requests
import os
from bs4 import BeautifulSoup # Added for HTML parsing

# Note: Loading the same file into bill_data and session_data.
# Ensure the JSON structure is suitable for both functions' needs.
# For bill_data, get_bill_data expects a list of dicts with "Session" and "Bill Number".
# For session_data, get_session_id expects a list of dicts with "Session" and optionally "Session_ID".
try:
    with open("/Users/justin/Desktop/GitHub.nosync/State_Legislation/pennsylvania_session_ids.json", "r") as file:
        bill_data = json.load(file)
except FileNotFoundError:
    print("Error: pennsylvania_session_ids.json not found. get_bill_data will not function correctly.")
    bill_data = [] # Initialize to empty list to prevent further errors if file not found
except json.JSONDecodeError:
    print("Error: pennsylvania_session_ids.json is not valid JSON.")
    bill_data = []


try:
    with open("/Users/justin/Desktop/GitHub.nosync/State_Legislation/pennsylvania_session_ids.json", "r") as file:
        session_data = json.load(file)
except FileNotFoundError:
    print("Error: pennsylvania_session_ids.json not found. get_session_id may not function as expected.")
    session_data = [] # Initialize to empty list
except json.JSONDecodeError:
    print("Error: pennsylvania_session_ids.json is not valid JSON.")
    session_data = []


def get_bill_data(session, bill_number):
    """Checks if the session and bill number exist in the loaded bill_data."""
    if not bill_data: # If bill_data failed to load or is empty
        print("Warning: bill_data is empty or not loaded. Cannot verify bill existence.")
        return True # Or False, depending on desired behavior when manifest is missing
    return any(entry.get("Session") == session and entry.get("Bill Number") == bill_number for entry in bill_data)

def parse_session_years(session_id_str):
    """
    Parse the session identifier string to extract individual years.
    Expected format: "1995-1996 Regular" (or similar).
    """
    parts = session_id_str.split(" ")
    if parts:
        year_part = parts[0]
        if '-' in year_part:
            return year_part.split("-")
        else:
            return [year_part] # Handle single-year sessions like "2023 Regular"
    return []

def get_session_id(session_name):
    """
    Retrieve the session identifier from the session_data.
    If a dedicated session id (e.g., "Session_ID") is available, that is returned.
    Otherwise, as a fallback, the user-provided session name is used directly.
    """
    if not session_data: # If session_data failed to load or is empty
        print("Warning: session_data is empty or not loaded. Using session_name as session_id.")
        return session_name
    for sess_entry in session_data:
        if sess_entry.get("Session") == session_name:
            return sess_entry.get("Session_ID", session_name) # Use "Session_ID" if present, else session_name
    return session_name # Fallback if session_name not found in session_data

def write_file(file_name, directory, data):
    """Saves data to a JSON file in the specified directory."""
    dir_path = os.path.join("PA", "output", directory)
    os.makedirs(dir_path, exist_ok=True)
    file_path = os.path.join(dir_path, f"{file_name}.json")
    with open(file_path, 'w') as f:
        json.dump(data, f, indent=4)
    print(f"Saved: {file_path}")

def scrape_bill_metadata(uuid, state_abbr, bill_state_id, session_name_str):
    """
    Scrapes metadata for a given bill.
    Attempts to get JSON data first, then falls back to parsing HTML if necessary.
    """
    current_session_id = get_session_id(session_name_str)
    years_to_check = parse_session_years(current_session_id)
    
    formatted_bill_id_for_url = bill_state_id.replace(" ", "").lower()

    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
        'Accept': 'application/json, text/html, application/xhtml+xml, application/xml;q=0.9, image/webp, */*;q=0.8'
    }

    for yr_str in years_to_check:
        bill_url_path = f"https://www.palegis.us/legislation/bills/{yr_str}/{formatted_bill_id_for_url}"
        print(f"Attempting to fetch: {bill_url_path}")
        
        try:
            response = requests.get(bill_url_path, headers=headers, timeout=80)
            
            print(f"URL: {bill_url_path}, Status: {response.status_code}")
            response_content_type = response.headers.get('Content-Type', '').lower()
            print(f"Content-Type: {response_content_type}")

            response.raise_for_status() # Raise HTTPError for bad responses (4xx or 5xx)

            page_title_str = "N/A"
            page_description_str = "N/A"
            api_bill_id = "N/A" # Placeholder for billId from JSON API
            source_type_str = "N/A"

            if 'application/json' in response_content_type:
                source_type_str = "JSON API"
                page_json_data = response.json()
                if page_json_data:
                    page_title_str = page_json_data.get("title", "N/A")
                    page_description_str = page_json_data.get("description", "N/A")
                    api_bill_id = page_json_data.get("billId", "N/A")
                else:
                    print(f"JSON response for year {yr_str} was empty or null.")
                    continue # Try next year if JSON is empty

            elif response.status_code == 200 and 'text/html' in response_content_type:
                source_type_str = "HTML Scrape"
                print(f"Received HTML for year {yr_str}. Attempting to parse with BeautifulSoup.")
                soup = BeautifulSoup(response.text, 'lxml') # Or 'html.parser'

                # --- BEGIN BeautifulSoup Parsing Logic ---
                # CRITICAL: You MUST inspect the HTML of palegis.us bill pages
                # and update these selectors to accurately extract the data.
                # These are generic placeholders.

                # Attempt to get title from <title> HTML tag
                title_html_tag = soup.find('title')
                if title_html_tag:
                    page_title_str = title_html_tag.get_text(strip=True)
                    # Often the <title> tag includes the site name, e.g., "Bill Title - Pennsylvania General Assembly"
                    # You might want to clean it:
                    if " - Pennsylvania General Assembly" in page_title_str: # Example cleaning
                        page_title_str = page_title_str.replace(" - Pennsylvania General Assembly", "").strip()
                
                # Attempt to get description from <meta name="Description" content="...">
                meta_desc_tag = soup.find('meta', attrs={'name': 'Description'})
                if meta_desc_tag and meta_desc_tag.get('content'):
                    page_description_str = meta_desc_tag.get('content').strip()
                
                # If the above are too generic, you'll need more specific selectors:
                # e.g., h1_title_tag = soup.find('h1', class_='bill-page-title')
                # if h1_title_tag: page_title_str = h1_title_tag.get_text(strip=True)

                # Add more specific selectors for description if meta tag is not good enough
                # e.g. summary_div = soup.find('div', class_='bill-summary-class')
                # if summary_div: page_description_str = summary_div.get_text(strip=True)
                
                # --- END BeautifulSoup Parsing Logic ---
                
                if page_title_str == "N/A" and page_description_str == "N/A":
                    print(f"Could not extract meaningful title/description from HTML for {bill_url_path}. Trying next year if applicable.")
                    continue # Try next year

            else:
                print(f"Unsupported Content-Type '{response_content_type}' or unexpected status for {bill_url_path}. Trying next year.")
                continue # Try next year

            # If we successfully extracted data (either JSON or HTML)
            bill_metadata_dict = {
                "uuid": uuid,
                "state": state_abbr,
                "session": session_name_str,
                "state_bill_id": bill_state_id,
                "title": page_title_str,
                "description": page_description_str,
                "source_type": source_type_str,
                "state_url": bill_url_path # The URL that successfully yielded data
            }
            # For internal_id, prioritize JSON's billId. For HTML, it's less defined.
            internal_id_val = api_bill_id if source_type_str == "JSON API" else "N/A (HTML Source)"
            return bill_metadata_dict, internal_id_val

        except requests.exceptions.HTTPError as e:
            print(f"HTTP error for {bill_url_path}: {e}")
            # If 404, it's common, just try next year. For other errors, log and continue.
            if e.response.status_code == 404:
                print(f"Bill not found at {bill_url_path} (404).")
            else:
                # Log other HTTP errors more visibly or handle them differently if needed
                pass # Continue to next year or error out after loop
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON for {bill_url_path}: {e}")
            print(f"Response text (first 200 chars): {response.text[:200]}")
        except Exception as e: # Catch other errors like BS4 issues or general request exceptions
            print(f"An error occurred processing {bill_url_path}: {e}")
        
        print("-" * 20) # Separator between year attempts

    # If loop finishes without returning, means no year was successful
    print(f"Error: Could not fetch or parse bill metadata for {bill_state_id} in session {session_name_str} for any valid session year.")
    return {}, "N/A"


def scrape_single_bill(session_input_str, bill_number_input_str):
    """Main function to orchestrate scraping for a single bill."""
    # Validate against local manifest if desired (get_bill_data)
    # For this example, we'll proceed even if not in a local manifest,
    # but you might want to make get_bill_data's return mandatory.
    if not get_bill_data(session_input_str, bill_number_input_str):
        print(f"Info: '{session_input_str} - {bill_number_input_str}' not found in local JSON manifest (pennsylvania_session_ids.json). Attempting scrape anyway.")
        # If you require bills to be in the manifest, you could 'return' here.

    # Sanitize session string for UUID generation (basic example)
    safe_session_str = session_input_str.replace(' ', '').replace('-', '').replace('/', '_')
    bill_uuid = f"PA{safe_session_str}{bill_number_input_str.replace(' ', '')}"
    state_abbreviation = "PA"
    
    print(f"\nScraping: Session='{session_input_str}', Bill='{bill_number_input_str}', UUID='{bill_uuid}'")
    
    bill_meta_data, internal_id = scrape_bill_metadata(bill_uuid, state_abbreviation, bill_number_input_str, session_input_str)
    
    if bill_meta_data: # Check if metadata dictionary is not empty
        # Add internal_id to the metadata before saving, if it's meaningful
        bill_meta_data["internal_api_id"] = internal_id 
        write_file(bill_uuid, "bill_metadata", bill_meta_data)
    else:
        print(f"Failed to scrape metadata for {session_input_str} - {bill_number_input_str}.")


if __name__ == "__main__":
    session_main_input = input("Enter the session (e.g., 1995-1996 Regular): ").strip()
    bill_number_main_input = input("Enter the bill number (e.g., SB 1351): ").strip()
    
    if session_main_input and bill_number_main_input:
        scrape_single_bill(session_main_input, bill_number_main_input)
    else:
        print("Session and Bill Number cannot be empty.")