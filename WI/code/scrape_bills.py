import json
import requests
from datetime import datetime
import re
from concurrent.futures import ThreadPoolExecutor, as_completed
import csv
import os
import pandas as pd
import logging
from lxml import html
from dotenv import load_dotenv
from openai import OpenAI
import time

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

BASE_URL = "https://docs.legis.wisconsin.gov/"

motion_classifiers = {
    # Floor amendments & substitute amendments
    r"(Assembly|Senate)( substitute)? amendment": "amendment",
    r"(substitute )?amendment.*(adopted|offered|withdrawn|laid on table|placed)": "amendment",

    # Committee reports with vote counts
    r"Report.*(adoption|passage).*Ayes \d+, Noes \d+": "committee_vote",

    # Procedural motions
    r"laid on table, Ayes \d+, Noes \d+": "motion",
    r"refused to table.*, Ayes \d+, Noes \d+": "motion",
    r"refused to rerefer.*, Ayes \d+, Noes \d+": "motion",

    # Standard final passage actions
    r"Read a third time.*passed, Ayes \d+, Noes \d+": "passage",
    r"Read a third time": "passage",
    r"passed, Ayes \d+, Noes \d+": "passage",
    r"Adopted": "passage",
    r"concurred.*Ayes \d+, Noes \d+": "passage",
    r"Report (passage|concurrence)": "passage",

    # Conference report adoption
    r"Committee of Conference report.*Ayes \d+, Noes \d+": "passage",

    # Veto-related actions
    r"partial veto.*Ayes \d+, Noes \d+": "veto",

    # Catch-all numeric voting pattern
    r"Ayes \d+, Noes \d+": "vote"
}

load_dotenv()
OPENAI_APIKEY = os.getenv('OPENAI_APIKEY')
client = OpenAI(api_key=OPENAI_APIKEY)

def scrape_bill(uuid, state, state_bill_id, session):
    logging.info(f"Scraping bill {state_bill_id} for session {session} in state {state} with UUID {uuid}")
    bill_url = "https://docs.legis.wisconsin.gov/{}/proposals/{}".format(
        session,
        state_bill_id.lower()
    )

    logging.info(f"Fetching bill page from URL: {bill_url}")
    try:
        response = requests.get(bill_url, timeout=30)
        response.raise_for_status()
        tree = html.fromstring(response.content)
        logging.info(f"Successfully fetched bill page for {state_bill_id}")
    except requests.RequestException as e:
        logging.error(f"Failed to fetch bill page for {state_bill_id}: {e}")
        return

    try:
        metadata, link = scrape_bill_metadata(uuid, state, state_bill_id, session, tree, bill_url)
        append_to_csv(uuid, session, state_bill_id, link)
        write_file(uuid, "bill_metadata", metadata)

        sponsors_data = scrape_bill_sponsors(uuid, state, state_bill_id, session, tree)
        write_file(uuid, "sponsors", sponsors_data)

        history_data = scrape_bill_history(uuid, state, state_bill_id, session, tree)
        write_file(uuid, "bill_history", history_data)
        
        date_events = {}
        for voting in scrape_votes(uuid, state, state_bill_id, session, tree):
            voting_data, date = voting

            if date not in date_events:
                date_events[date] = 0

            file_name = "{}_{}_{}".format(uuid, date, date_events[date])
            date_events[date] += 1

            write_file(file_name, "votes", voting_data)

        time.sleep(10)
        
    except Exception as e:
        logging.error(f"Error processing bill {state_bill_id}: {e}")
        raise

def scrape_bill_metadata(uuid, state, state_bill_id, session, page, page_url):
    logging.info(f"Scraping metadata for bill {state_bill_id} in session {session} for state {state} with UUID {uuid}")
    
    bill_description = page.xpath("//div[@class='box-content']/div/p//text()")
    bill_description_clean = bill_description[-1].strip().capitalize() if bill_description else "No description available"

    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr/td[@class='entry']//text()")
    last_status = history[-1] if history else "Unknown status"

    bill_metadata = {
        "uuid": uuid,
        "state": state,
        "session": session,
        "state_bill_id": state_bill_id,
        "title": 'NA',
        "description": bill_description_clean,
        "status": last_status,
        "state_url": page_url,
    }

    links = page.xpath("//div[@class='propLinks noprint']/ul/li/p/span/a")
    url = None

    for link in links:
        text_elements = link.xpath("./text()")
        if text_elements and text_elements[0] == "Bill Text":
            href_elements = link.xpath("./@href")
            if href_elements:
                url = BASE_URL + href_elements[0]
                break

    return bill_metadata, url

def scrape_bill_sponsors(uuid, state, state_bill_id, session, page):
    logging.info(f"Scraping sponsors for bill {state_bill_id} in session {session} for state {state} with UUID {uuid}")
    sponsor_elements = page.xpath("//div[@class='propHistory']/table[@class='history']/tr/td[@class='entry']//text()")
    
    if not sponsor_elements:
        logging.warning(f"No sponsor information found for bill {state_bill_id}")
        return {
            "uuid": uuid,
            "state": state,
            "session": session,
            "state_bill_id": state_bill_id,
            "sponsors": []
        }

    sponsors = sponsor_elements[0]

    if ";" in sponsors:
        lines = sponsors.split(";")
    else:
        lines = [sponsors]

    sponsor_list = []
    for line in lines:
        match = re.match(
            r"(Introduced|cosponsored|Cosponsored) by (?:joint )?(Senator|Representative|committee|Joint Legislative Council|Law Revision Committee)s?(.*)",
            line.strip(),
        )

        if match:
            type_sponsor, _, people = match.groups()

            if type_sponsor == "Introduced":
                sponsor_type = "sponsor"
            elif type_sponsor in ["Cosponsored", "cosponsored"]:
                sponsor_type = "cosponsor"
            else:
                sponsor_type = "unknown"

            for r in re.split(r"\sand\s|\,", people):
                if r.strip():
                    sponsor_list.append({
                        "sponsor_name": r.strip().rstrip('.'),
                        "sponsor_type": sponsor_type,
                    })

    sponsor_data = {
        "uuid": uuid,
        "state": state,
        "session": session,
        "state_bill_id": state_bill_id,
        "sponsors": sponsor_list
    }

    return sponsor_data

def scrape_bill_history(uuid, state, state_bill_id, session, page):
    logging.info(f"Scraping history for bill {state_bill_id} in session {session} for state {state} with UUID {uuid}")
    
    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr")

    actions = []
    for event in history:
        date_elements = event.xpath("./td[@class='date']/text()")
        house_elements = event.xpath("./td[@class='date']/abbr/text()")
        
        if not date_elements or not house_elements:
            continue
            
        date = date_elements[0].strip()
        try:
            date_object = datetime.strptime(date, "%m/%d/%Y")
            formatted_date = date_object.strftime("%Y-%m-%d")
        except ValueError:
            logging.warning(f"Invalid date format: {date}")
            continue

        house = house_elements[0]
        event_description = "".join(event.xpath("./td[@class='entry']//text()"))
        event_description_clean = house + " - " + event_description

        action_data = {
            "date": formatted_date,
            "action": event_description_clean,
        }

        actions.append(action_data)

    bill_history_data = {
        "uuid": uuid,
        "state": state,
        "session": session,
        "state_bill_id": state_bill_id,
        "history": actions
    }

    return bill_history_data

def scrape_votes(uuid, state, state_bill_id, session, page):
    logging.info(f"Scraping votes for bill {state_bill_id} in session {session} for state {state} with UUID {uuid}")
    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr")

    voting_events = set()

    # getting all links to references
    for event in history:
        event_description = "".join(event.xpath("./td[@class='entry']//text()"))

        for regex, _ in motion_classifiers.items():
            if re.search(regex, event_description):  # Changed from re.match to re.search
                reference_elements = event.xpath("./td[@class='journal noprint']/a/@href")
                
                if not reference_elements:
                    continue
                    
                reference = reference_elements[0]
                house_elements = event.xpath("./td[@class='date']/abbr/text()")
                date_elements = event.xpath("./td[@class='date']/text()")
                
                if not house_elements or not date_elements:
                    continue

                house = house_elements[0][0]
                date = date_elements[0].strip()
                
                try:
                    date_object = datetime.strptime(date, "%m/%d/%Y")
                    formatted_date = date_object.strftime("%Y-%m-%d")
                except ValueError:
                    logging.warning(f"Invalid date format: {date}")
                    continue

                voting_events.add((reference, formatted_date, house))

    questions = set()
    vote_events = []

    # Ensure scratch directory exists
    os.makedirs("WI/scratch", exist_ok=True)

    for voting_event, date, chamber in voting_events:
        try:
            url = voting_event
            response = requests.get(url, timeout=30)
            response.raise_for_status()
            page = html.fromstring(response.content)

            text = "".join(page.xpath("//div[@class='journals']//text()"))

            journal_page_filepath = f"WI/scratch/{voting_event[-4:]}.txt"
            with open(journal_page_filepath, "w", encoding="utf-8") as file:
                file.write(text)

            data = scrapeWithOpenAI(state_bill_id, journal_page_filepath)

            for event in data:
                if event['description'] not in questions:
                    vote_event = {
                        "uuid": uuid,
                        "state": state,
                        "session": session,
                        "state_bill_id": state_bill_id,
                        "chamber": chamber,
                        "date": date,
                        "description": event['description'],
                        "yeas": event['yeas'],
                        "nays": event['nays'],
                        "other": event['other'],
                        "roll_call": event['roll_call']
                    }

                    vote_events.append((vote_event, date))
                    questions.add(event['description'])

            # Clean up temporary file
            if os.path.exists(journal_page_filepath):
                os.remove(journal_page_filepath)
                
        except Exception as e:
            logging.error(f"Error processing voting event {voting_event}: {e}")
            continue
    
    return vote_events

def append_to_csv(uuid, session, bill_number, link):
    filename = "WI/scratch/bills.csv"
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    file_exists = os.path.isfile(filename)

    with open(filename, mode="a", newline="", encoding="utf-8") as file:
        writer = csv.writer(file)

        if not file_exists:
            writer.writerow(["uuid", "session", "bill_number", "link"])

        writer.writerow([uuid, session, bill_number, link])

def write_file(file_name, directory, data):
    logging.info(f"Writing data to file: {file_name} in directory: {directory}")
    output_dir = f'./WI/output/{directory}'
    os.makedirs(output_dir, exist_ok=True)

    file_path = os.path.join(output_dir, f"{file_name}.json")
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=4, ensure_ascii=False)

def scrapeWithOpenAI(bill_id, file_path):
    congress = {
        "H": "House",
        "S": "Senate",
        "A": "Assembly",
    }

    try:
        with open(file_path, "r", encoding="utf-8") as file:
            file_content = file.read()
    except FileNotFoundError:
        logging.error(f"File not found: {file_path}")
        return []

    prompt = f"""Extract all voting-related actions for {congress.get(bill_id[0], 'Unknown')} Bill {bill_id[2:]} ({bill_id}) from this text. Identify votes for this bill only. All other bills should be ignored.
            {file_content}

            Identify each case where:
            - A question resulted in 'Motion carried' or includes recorded votes, OR
            - The bill was "Read for a third time and passed"

            For each of these cases, return the following structured information:

            - The exact question text, if present.
            If it states "Read for a third time and passed" with no explicit question, use:
            "question": "Read for a third time and passed"

            - The 'ayes' field:
            - If the result was 'Motion carried', 'Read for a third time and passed', or 'Adopted' **without any explicit list of voters**, return "ayes": "unanimous"
            - If a roll-call vote is recorded, return the full list of names under "ayes"

            - The full list of 'noes', if applicable.

            - The list of absent or not voting members, if applicable.

            ### **Expected JSON Output Format:**
            
            {{
            "votes": [
                {{
                "question": "Exact question text",
                "ayes": "unanimous",
                "noes": [],
                "absent_or_not_voting": []
                }},
                {{
                "question": "Another exact question",
                "ayes": ["Name1", "Name2"],
                "noes": ["NameX"],
                "absent_or_not_voting": ["NameZ"]
                }}
            ]
            }}
            """

    try:
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            temperature=0,
            messages=[
                {"role": "system", "content": "You are a helpful assistant that extracts structured voting information from text."},
                {"role": "user", "content": prompt},
            ]
        )

        raw_response = response.choices[0].message.content.strip()
        # Remove markdown code block markers
        cleaned_response = re.sub(r'```json\n?|```\n?', '', raw_response).strip()

        parsed_data = json.loads(cleaned_response)

        questions = set()
        voting_event = []
        
        for vote in parsed_data.get("votes", []):
            question = vote.get("question", "")
            if question and question not in questions:
                questions.add(question)
                voter = []

                ayes = vote.get("ayes", [])
                noes = vote.get("noes", [])
                absent = vote.get("absent_or_not_voting", [])

                if isinstance(ayes, list):
                    for name in ayes:
                        voter.append({
                            "name": name,
                            "response": "Yea"
                        })

                    for name in noes:
                        voter.append({
                            "name": name,
                            "response": "Nay"
                        })

                    for name in absent:
                        voter.append({
                            "name": name,
                            "response": "NV"
                        })

                    event = {
                        "description": question,
                        "yeas": len(ayes),
                        "nays": len(noes),
                        "other": len(absent),
                        "roll_call": voter
                    }

                    voting_event.append(event)
                else:
                    event = {
                        "description": question,
                        "yeas": "unanimous",
                        "nays": 0,
                        "other": 0,
                        "roll_call": "unanimous"
                    }

                    voting_event.append(event)

        return voting_event

    except json.JSONDecodeError as e:
        logging.error(f"JSON decode error: {e}")
        logging.error(f"Raw response: {cleaned_response}")
        return []
    except Exception as e:
        logging.error(f"Error in OpenAI processing: {e}")
        return []

if __name__ == "__main__":
    print('Starting bill scrape...')
    
    try:
        bill_list = pd.read_csv("WI/output/WI_bills_to_process.csv")
        metadata_dir = "WI/output/bill_metadata"
        
        # Create directory if it doesn't exist
        os.makedirs(metadata_dir, exist_ok=True)
        
        # Get existing UUIDs
        existing_uuids = set()
        if os.path.exists(metadata_dir):
            existing_uuids = {filename.removesuffix(".json") for filename in os.listdir(metadata_dir) if filename.endswith(".json")}

        # Exclude bills that have already been processed
        bill_list = bill_list[~bill_list["UUID"].isin(existing_uuids)]
        bill_rows = bill_list[["UUID", "session", "bill_number"]].to_dict(orient="records")
        
        print(f"Processing {len(bill_rows)} bills...")
        
        # Parallel execution with proper completion handling
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = {
                executor.submit(scrape_bill, row["UUID"], "WI", row["bill_number"], row["session"]): row
                for row in bill_rows
            }

            # Wait for all futures to complete and handle results/exceptions
            for future in as_completed(futures):
                row = futures[future]
                try:
                    result = future.result()
                    logging.info(f"Successfully processed bill {row['bill_number']} (UUID: {row['UUID']})")
                except Exception as exc:
                    logging.error(f"Bill {row} generated an exception: {exc}")
        
        print("All bills processed successfully!")
        
    except FileNotFoundError as e:
        logging.error(f"Required file not found: {e}")
        print("Error: Required files not found. Please check file paths.")
    except Exception as e:
        logging.error(f"Unexpected error: {e}")
        print(f"An unexpected error occurred: {e}")