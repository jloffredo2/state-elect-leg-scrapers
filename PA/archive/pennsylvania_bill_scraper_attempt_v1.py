import json
import requests
import os
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import re
import sys

bill_data = []
session_data = []

file_path = '/Users/justin/Desktop/GitHub.nosync/State_Legislation/state-elect-leg-scrapers/PA/pennsylvania_session_ids.json'

# def prompt_and_load_json():
#     """
#     Prompts the user for the path to the JSON file and loads it.
#     It will keep prompting until a valid file path and JSON are provided.

#     Returns:
#         The loaded data from the JSON file, or None if loading fails.
#     """
#     while True:
#         file_path = input("Please enter the full path to the pennsylvania_session_ids.json file: ").strip()
#         if not file_path:
#             print("Path cannot be empty. Please try again.")
#             continue
        
#         if file_path.startswith(('"', "'")) and file_path.endswith(('"', "'")):
#             file_path = file_path[1:-1]

#         try:
#             with open(file_path, "r") as file:
#                 data = json.load(file)
#             print(f"Successfully loaded data from: {file_path}")
#             return data
#         except FileNotFoundError:
#             print(f"Error: The file was not found at '{file_path}'. Please check the path and try again.")
#         except json.JSONDecodeError:
#             print(f"Error: The file at '{file_path}' is not a valid JSON file. Please check the file content.")
#         except Exception as e:
#             print(f"An unexpected error occurred: {e}")
        
#         retry = input("Try again? (y/n): ").lower()
#         if retry != 'y':
#             return None


def get_bill_data(session, bill_number):
    if not bill_data:
        print("Warning: bill_data is empty or not loaded. Cannot verify bill existence.")
        return True 
    return any(entry.get("Session") == session and entry.get("Bill Number") == bill_number for entry in bill_data)

def parse_session_years(session_id_str):
    parts = session_id_str.split(" ")
    if parts:
        year_part = parts[0]
        if '-' in year_part:
            return year_part.split("-")
        else:
            return [year_part]
    return []

def get_session_id(session_name):
    if not session_data:
        print("Warning: session_data is empty or not loaded. Using session_name as session_id.")
        return session_name
    for sess_entry in session_data:
        if sess_entry.get("Session") == session_name:
            return sess_entry.get("Session_ID", session_name)
    return session_name

def write_file(file_name_without_extension, data_to_save):
    """
    Saves data to a JSON file in the same directory as the script.
    The 'original_subfolder_name_arg' is now ignored for path creation but kept for signature compatibility.
    """
    try:
        script_dir = os.path.dirname(os.path.abspath(__file__))
    except NameError:
        script_dir = os.getcwd()
        print(f"Warning: __file__ not defined, saving to current working directory: {script_dir}")

    file_path = os.path.join(script_dir, f"{file_name_without_extension}.json")

    try:
        with open(file_path, 'w') as f:
            json.dump(data_to_save, f, indent=4)
        print(f"Saved: {file_path}")
    except IOError as e:
        print(f"Error saving file {file_path}: {e}")
    except Exception as e:
        print(f"An unexpected error occurred while saving {file_path}: {e}")

def bill_text_html(bill_page_soup, base_bill_url, headers):
    """
    Finds a link to the full bill text on the page and fetches its HTML content,
    using specific attributes from the provided HTML snippet.

    Args:
        bill_page_soup: BeautifulSoup object of the main bill information page.
        base_bill_url: The URL of the main bill information page (for resolving relative links).
        headers: The headers to use for the request.

    Returns:
        The HTML content of the full bill text as a string, or None if not found.
    """
    print(f"Searching for specific bill text link on page: {base_bill_url}")
    bill_text_html_content = None
    bill_text_link_tag = None

    bill_text_link_tag = bill_page_soup.find('a', attrs={'data-bs-original-title': 'HTML Bill Text'})

    if not bill_text_link_tag:
        bill_text_link_tag = bill_page_soup.find('a', href=lambda href: href and "/legislation/bills/text/HTM/" in href)
    

    if bill_text_link_tag and bill_text_link_tag.get('href'):
        # The 'href' attribute in the snippet is the primary one.
        # 'data-uw-original-href' is also present and identical, likely for a specific JavaScript functionality on the site.
        bill_text_url_relative = bill_text_link_tag.get('href')
        
        bill_text_url_absolute = urljoin(base_bill_url, bill_text_url_relative)
        
        print(f"Found bill text link (using data-bs-original-title or href): {bill_text_link_tag.get_text(strip=True) or '[Icon Link]'} -> {bill_text_url_absolute}")
        
        try:
            response = requests.get(bill_text_url_absolute, headers=headers, timeout=60)
            response.raise_for_status()
            
            if 'text/html' in response.headers.get('Content-Type', '').lower():
                bill_text_html_content = response.text
                print(f"Successfully fetched HTML bill text from {bill_text_url_absolute}")
            else:
                print(f"Link found, but content at {bill_text_url_absolute} is not HTML (Content-Type: {response.headers.get('Content-Type')}).")
        except requests.exceptions.RequestException as e:
            print(f"Error fetching bill text from {bill_text_url_absolute}: {e}")
    else:
        print(f"Could not find the specific link to the full bill text on {base_bill_url} using provided attributes.")
        
    return bill_text_html_content

def extract_text_from_pre_tags(html_content):
    if not html_content or html_content == "N/A":
        return "N/A"
    soup = BeautifulSoup(html_content, 'lxml')
    
    pre_tags = soup.find_all('pre')
    
    if not pre_tags:
        body = soup.find('body')
        if body:
            return body.get_text(separator='\n', strip=True)
        return soup.get_text(separator='\n', strip=True)

    bill_text_parts = []
    for pre_tag in pre_tags:
        bill_text_parts.append(pre_tag.get_text(strip=False))

    full_pre_text = "".join(bill_text_parts)
    
    lines = full_pre_text.splitlines()
    cleaned_lines = []
    for line in lines:
        stripped_line = line.strip()
        if "PRINTER'S NO." in stripped_line or re.match(r'^\d{4}[HS]\d+B\d+\s+-\s*\d+\s*-$', stripped_line):
            continue
        cleaned_lines.append(line) 
        
    return "\n".join(cleaned_lines).strip()


def scrape_bill_metadata(uuid, state_abbr, bill_state_id, session_name_str):
    
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

            response.raise_for_status()

            sponsors_list = []
            actions_list = []
            if 'application/json' in response_content_type:
                page_json_data = response.json()
                if not page_json_data:
                    print(f"JSON response for year {yr_str} was empty or null.")
                    continue

            elif response.status_code == 200 and 'text/html' in response_content_type:
                print(f"Received HTML for year {yr_str}. Attempting to parse with BeautifulSoup.")
                soup = BeautifulSoup(response.text, 'lxml')                
                
                try:
                    text_content = soup.get_text('\n')
                    lines = [line.strip() for line in text_content.splitlines() if line.strip()]

                    try:
                        sponsors_keyword_idx = lines.index("Sponsors")
                        if sponsors_keyword_idx + 1 < len(lines):
                            sponsor_line_text = lines[sponsors_keyword_idx + 1]
                            sponsors_list = [name.strip().title() for name in sponsor_line_text.split(',') if name.strip()]
                    except ValueError:
                        print(f"'Sponsors' not found in {bill_url_path}.")
                    except Exception as e_spons:
                        print(f"Error processing sponsors {bill_url_path}: {e_spons}")

                    # Extract Actions (page.py)
                    try:
                        actions_keyword_idx = lines.index("Actions")
                        temp_actions = []
                        for line_num in range(actions_keyword_idx + 1, len(lines)):
                            action_line_text = lines[line_num]
                            if action_line_text.startswith("Generated"):
                                break
                            temp_actions.append(action_line_text)
                        actions_list = temp_actions
                    except ValueError:
                        print(f"Keyword 'Actions' not found in HTML text lines for {bill_url_path}.")
                    except Exception as e_act:
                        print(f"Error processing actions from HTML lines for {bill_url_path}: {e_act}")

                except Exception as e_text_proc:
                    print(f"Error processing text content from HTML for {bill_url_path} (for sponsors/actions): {e_text_proc}")
                if not sponsors_list and not actions_list:
                    print(f"Could not extract meaningful data (title, desc, sponsors, or actions) from HTML for {bill_url_path}. Trying next year if applicable.")
                    continue

            else:
                print(f"Unsupported Content-Type '{response_content_type}' or unexpected status for {bill_url_path}. Trying next year.")
                continue

            
            bill_html_path = bill_text_html(soup, bill_url_path, headers)

            bill_metadata_dict = {
                "uuid": uuid,
                "state": state_abbr,
                "session": session_name_str,
                "state_bill_id": bill_state_id,
                "sponsors": sponsors_list,
                "actions": actions_list,
                "state_url": bill_url_path,
                "bill_text_html": bill_html_path
            }
            internal_id_val = "(HTML Source)"
            return bill_metadata_dict, internal_id_val
        
        except requests.exceptions.HTTPError as e:
            print(f"HTTP error for {bill_url_path}: {e}")
            if e.response.status_code == 404:
                print(f"Bill not found at {bill_url_path} (404).")
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON for {bill_url_path}: {e}. Response text (first 200 chars): {response.text[:200]}")
        except Exception as e:
            print(f"An error occurred processing {bill_url_path}: {e}")
        
        print("-" * 20)

    print(f"Error: Could not fetch or parse bill metadata for {bill_state_id} in session {session_name_str} for any valid session year.")
    return {}, "N/A"


def scrape_single_bill(session_input_str, bill_number_input_str):
    global bill_data, session_data 

    if not get_bill_data(session_input_str, bill_number_input_str):
        print(f"Info: '{session_input_str} - {bill_number_input_str}' not found in local JSON manifest. Attempting scrape anyway.")

    safe_session_str = session_input_str.replace(' ', '').replace('-', '').replace('/', '_')
    bill_uuid = f"PA{safe_session_str}{bill_number_input_str.replace(' ', '')}"
    state_abbreviation = "PA"
    
    print(f"\nScraping: Session='{session_input_str}', Bill='{bill_number_input_str}', UUID='{bill_uuid}'")
    
    bill_metadata, internal_id = scrape_bill_metadata(bill_uuid, state_abbreviation, bill_number_input_str, session_input_str) 

    raw_bill_html = bill_metadata.pop("bill_text_html", None)

    if raw_bill_html and raw_bill_html != "N/A":
        cleaned_bill_text = extract_text_from_pre_tags(raw_bill_html)
        bill_metadata["cleaned_bill_text"] = cleaned_bill_text
    else:
        bill_metadata["cleaned_bill_text"] = "N/A"


    bill_metadata["internal_api_id"] = internal_id 
    write_file(bill_uuid, bill_metadata)



if __name__ == "__main__":

    # try:
    #     # This will show the full path to the requests package being used
    #     requests_path = requests.__file__
    #     print(f"requests module is imported from: {requests_path}")

    #     # This often reveals the bin/Scripts directory of the active virtual environment
    #     # or the system Python's bin directory.
    #     python_executable = sys.executable
    #     print(f"Python interpreter being used: {python_executable}")

    #     # If you're in a virtual environment, its base path is often found here
    #     # (though sys.executable is usually more direct for the interpreter)
    #     # This might give you the path to the environment root.
    #     print(f"sys.prefix (environment root): {sys.prefix}")

    #     # Check if a virtual environment is active (more of a hint)
    #     if hasattr(sys, 'real_prefix') or (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix):
    #         print("A virtual environment appears to be active.")
    #     else:
    #         print("No virtual environment appears to be active (might be system Python).")

    # except ImportError:
    #     print("Error: 'requests' module not found.")
    # except Exception as e:
    #     print(f"An unexpected error occurred: {e}")

    # loaded_data = prompt_and_load_json()

    with open(file_path, 'r') as file:
        loaded_data = json.load(file)

    if loaded_data:
        bill_data = loaded_data
        session_data = loaded_data

        session_main_input = input("Enter the session (e.g., 1995-1996 Regular): ").strip()
        bill_number_main_input = input("Enter the bill number (e.g., SB 1351): ").strip()
        
        if session_main_input and bill_number_main_input:
            try:
                import lxml 
            except ImportError:
                print("\nWarning: lxml parser not found. HTML parsing might be slower or fail for complex pages.")
                print("Consider installing it with: pip install lxml")
                print("Ensure your BeautifulSoup constructor uses 'lxml' or change it to 'html.parser'.\n")

            scrape_single_bill(session_main_input, bill_number_main_input)
        else:
            print("Session and Bill Number cannot be empty.")
    else:
        print("\nCould not load session data. Exiting script.")