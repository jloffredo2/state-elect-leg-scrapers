import json
import requests
import os

with open("/Users/justin/Desktop/GitHub.nosync/State_Legislation/pennsylvania_session_ids.json", "r") as file:
    bill_data = json.load(file)

with open("/Users/justin/Desktop/GitHub.nosync/State_Legislation/pennsylvania_session_ids.json", "r") as file:
    session_data = json.load(file)

def get_bill_data(session, bill_number):
    return any(entry["Session"] == session and entry["Bill Number"] == bill_number for entry in bill_data)

def parse_session_years(session_id):
    """
    Parse the session identifier to extract individual years.
    Expected format: "1995-1996 Regular" (or similar).
    This function takes the first space-delimited token (e.g., "1995-1996")
    and splits it on '-' to return the list of years.
    """
    parts = session_id.split(" ")
    if parts:
        year_part = parts[0]
        if '-' in year_part:
            return year_part.split("-")
        else:
            return [year_part]
    return []

def get_session_id(session_name):
    """
    Retrieve the session identifier from the session_data.
    If a dedicated session id (e.g., "Session_ID") is available, that is returned.
    Otherwise, as a fallback, the user-provided session name (e.g., "1995-1996 Regular")
    is used directly.
    """
    for sess in session_data:
        if sess.get("Session") == session_name:
            return sess.get("Session_ID", session_name)
    return session_name

def write_file(file_name, directory, data):
    dir_path = os.path.join("PA", "output", directory)
    os.makedirs(dir_path, exist_ok=True)
    file_path = os.path.join(dir_path, f"{file_name}.json")
    with open(file_path, 'w') as f:
        json.dump(data, f, indent=4)
    print(f"Saved: {file_path}")

def scrape_bill(session, bill_number):
    if not get_bill_data(session, bill_number):
        print("Error: Session and Bill Number not found in JSON file.")
        return

    uuid = f"PA{session.replace(' ', '')}{bill_number.replace(' ', '')}"
    state = "PA"
    
    bill_meta_data, internal_id = scrape_bill_metadata(uuid, state, bill_number, session)
    write_file(uuid, "bill_metadata", bill_meta_data)

def scrape_bill_metadata(uuid, state, state_bill_id, session):
    session_id = get_session_id(session)
    years = parse_session_years(session_id)
    bill_metadata = {}
    working_year = None

    formatted_bill_id = state_bill_id.replace(" ", "").lower()

    for yr in years:
        bill_json_url = f"https://www.palegis.us/legislation/bills/{yr}/{formatted_bill_id}"
        try:
            response = requests.get(bill_json_url, timeout=80)
            response.raise_for_status()
            page = response.json()
            if page:
                working_year = yr
                bill_metadata = {
                    "uuid": uuid,
                    "state": state,
                    "session": session,
                    "state_bill_id": state_bill_id,
                    "title": page.get("title", "N/A"),
                    "description": page.get("description", "N/A"),
                    # status, act. no => chaptered
                    # use last action
                    "state_url": f"https://www.palegis.us/legislation/bills/{yr}/{formatted_bill_id}"
                }
                break
            
            elif response.status_code == 200 and 'text/html' in content_type:
                print(f"Received HTML for year {yr} from {bill_url}. Attempting to parse with BeautifulSoup.")
                soup = BeautifulSoup(response.text, 'lxml') # Or 'html.parser'

                page_title = "N/A"
                page_description = "N/A"

                # --- ADAPT YOUR LOGIC FROM page.py HERE ---
                # Example for Title (you'll need to find the actual selectors)
                # Based on common HTML, a title might be in an <h1> or <title> tag
                title_tag = soup.find('title') # Gets the <title>...</title> element
                if title_tag:
                    page_title = title_tag.get_text(strip=True) 
                    # You might need to clean it further if it includes site name, etc.

                # Example for Description (you'll need to find actual selectors)
                # Often in a <meta name="description" content="..."> tag or a specific <div>
                meta_desc = soup.find('meta', attrs={'name': 'Description'})
                if meta_desc and meta_desc.get('content'):
                    page_description = meta_desc.get('content').strip()
                else:
                    # Add other attempts to find description if needed
                    pass 

                # If you want to include actions like in page.py:
                # adapted_actions = []
                # try:
                #     act_idx = lines.index("Actions") # You'd need to get 'lines' as in page.py
                #     # ... and then extract actions ...
                # except ValueError:
                #    pass # Handle if "Actions" not found
                # --- END ADAPTATION ---

                if page_title != "N/A" or page_description != "N/A":
                    bill_metadata_payload = {
                        "uuid": uuid,
                        "state": state,
                        "session": session,
                        "state_bill_id": state_bill_id,
                        "title": page_title,
                        "description": page_description,
                        # "actions": adapted_actions, # If you add actions
                        "source_type": "HTML Scrape",
                        "state_url": bill_url
                    }
                    return bill_metadata_payload, "N/A (HTML Source)"
                else:
                    print(f"Could not extract meaningful data from HTML for {bill_url}")

        except requests.RequestException as e:
            print(f"Error fetching metadata for year {yr}: {e}")

    if working_year is None:
        print("Error fetching bill metadata for any valid session year.")
        return {}, "N/A"
    
    return bill_metadata, page.get("billId", "N/A")

if __name__ == "__main__":
    session = input("Enter the session: ").strip()
    bill_number = input("Enter the bill number: ").strip()
    
    scrape_bill(session, bill_number)
