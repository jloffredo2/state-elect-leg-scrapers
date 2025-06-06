
import requests
from datetime import datetime
import re
import json

import csv
import os

from lxml import html
from dotenv import load_dotenv
from openai import OpenAI

BASE_URL = "https://docs.legis.wisconsin.gov/"

motion_classifiers = {
    "(Assembly|Senate)( substitute)? amendment": "amendment",
    "Report (passage|concurrence)": "passage",
    "Report (adoption|introduction and adoption) of Senate( Substitute)? Amendment": "amendment",
    "Report Assembly( Substitute)? Amendment": "amendment",
    "Read a third time": "passage",
    "Adopted": "passage",
}

load_dotenv()
OPENAI_APIKEY = os.getenv('OPENAI_APIKEY')
client = OpenAI(api_key=OPENAI_APIKEY)

def scrape_bill(uuid, state, state_bill_id, session):
    bill_url = (
        "https://docs.legis.wisconsin.gov/{}/proposals/{}".format(session, state_bill_id)
    )
    response = requests.get(bill_url)
    tree = html.fromstring(response.content)


    date_events = {}
    for voting in scrape_votes(uuid, state, state_bill_id, session, tree):
        voting_data, date = voting

        if date not in date_events:
            date_events[date] = 0

        file_name = "{}_{}_{}".format(uuid, date, date_events[date])
        date_events[date] += 1

        write_file(file_name, "votes", voting_data)

    metadata, link = scrape_bill_metadata(uuid, state, state_bill_id, session, tree, bill_url)
    append_to_csv(uuid, session, state_bill_id, link)
    write_file(uuid, "bill_metadata", metadata)

    sponsors_data = scrape_bill_sponsors(uuid, state, state_bill_id, session, tree)
    write_file(uuid, "sponsors", sponsors_data)

    history_data = scrape_bill_history(uuid, state, state_bill_id, session, tree)
    write_file(uuid, "bill_history", history_data)

def scrape_bill_metadata(uuid, state, state_bill_id, session, page, page_url):
    bill_description = page.xpath("//div[@class='box-content']/div/p//text()")
    bill_description_clean = bill_description[-1].strip().capitalize()

    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr/td[@class='entry']//text()")
    last_status = history[-1]

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

    for link in links:
        text = link.xpath("./text()")[0]

        if text == "Bill Text":
            href = link.xpath("./@href")[0]
            url =  BASE_URL + href

    return bill_metadata, url

def scrape_bill_sponsors(uuid, state, state_bill_id, session, page):
    sponsors = page.xpath("//div[@class='propHistory']/table[@class='history']/tr/td[@class='entry']//text()")[0]

    if ";" in sponsors:
        lines = sponsors.split(";")
    else:
        lines = [sponsors]

    sponsor_list = []
    for line in lines:
        match = re.match(
                    "(Introduced|cosponsored|Cosponsored) by (?:joint )?(Senator|Representative|committee|Joint Legislative Council|Law Revision Committee)s?(.*)",
                    line.strip(),
                )

        type, _, people = match.groups()

        if type == "Introduced":
            sponsor_type = "sponsor"
        elif type == "Cosponsored" or type == "cosponsored":
            sponsor_type = "cosponsor"

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
    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr")

    actions = []
    for event in history:
        date = event.xpath("./td[@class='date']/text()")[0].strip()
        date_object = datetime.strptime(date, "%m/%d/%Y")
        formatted_date = date_object.strftime("%Y-%m-%d")

        house = event.xpath("./td[@class='date']/abbr/text()")[0]

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
    history = page.xpath("//div[@class='propHistory']/table[@class='history']/tr")

    voting_events = set()

    # getting all links to references
    for event in history:
        event_description = "".join(event.xpath("./td[@class='entry']//text()"))

        for regex, _ in motion_classifiers.items():
            if re.match(regex, event_description):
                reference = event.xpath("./td[@class='journal noprint']/a/@href")[0]

                house = event.xpath("./td[@class='date']/abbr/text()")[0][0]

                date = event.xpath("./td[@class='date']/text()")[0].strip()
                date_object = datetime.strptime(date, "%m/%d/%Y")
                formatted_date = date_object.strftime("%Y-%m-%d")

                voting_events.add((reference, formatted_date, house))

    questions = set()
    vote_events = []

    for voting_event, date, chamber in voting_events:
        url = voting_event
        response = requests.get(url)
        page = html.fromstring(response.content)

        # checking for important votes
        # if page.xpath("//div[@class='qs_entry_']//a//@href"):
        text = "".join(page.xpath("//div[@class='journals']//text()"))

        journal_page_filepath = f"WI/scratch/{voting_event[-4:]}.txt"
        file = open(journal_page_filepath, "w")
        file.write(text)
        file.close()

        data = scrapeWithOpenAI(state_bill_id, f"WI/scratch/{voting_event[-4:]}.txt")

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

            else:
                raise TypeError("Present member not found")

    os.remove(journal_page_filepath)
    
    return vote_events

def append_to_csv(uuid, session, bill_number, link):
    filename = "WI/scratch/bills.csv"
    file_exists = os.path.isfile(filename)

    with open(filename, mode="a", newline="") as file:
        writer = csv.writer(file)

        if not file_exists:
            writer.writerow(["uuid", "session", "bill_number", "link"])

        writer.writerow([uuid, session, bill_number, link])

def write_file(file_name, directory, data):
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

    with open(file_path, "r", encoding="utf-8") as file:
        file_content = file.read()

    prompt = f"""Extract all voting-related actions for {congress[bill_id[0]]} Bill {bill_id[2:]} ({bill_id}) from this text. Identify votes for this bill only. All other bills should be ignored.

{file_content}

Identify each case where:
- A question resulted in 'Motion carried' or includes recorded votes, OR
- The bill was "Read for a third time and passed"

For each of these cases, return the following structured information:

- The exact question text, if present.
  If it states "Read for a third time and passed" with no explicit question, use:
  `"question": "Read for a third time and passed"`

- The 'ayes' field:
  - If the result was 'Motion carried', 'Read for a third time and passed', or 'Adopted' **without any explicit list of voters**, return `"ayes": "unanimous"`
  - If a roll-call vote is recorded, return the full list of names under `"ayes"`

- The full list of 'noes', if applicable.

- The list of absent or not voting members, if applicable.

### **Expected JSON Output Format:**
```json
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

    response = client.chat.completions.create(
        model="gpt-4o-mini",
        temperature=0,
        messages=[
            {"role": "system", "content": "You are a helpful assistant that extracts structured voting information from text."},
            {"role": "user", "content": prompt},
        ]
    )

    raw_response = response.choices[0].message.content.strip()
    cleaned_response = re.sub(r"```json|```", "", raw_response).strip()

    try:
        parsed_data = json.loads(cleaned_response)

        questions = set()
        voting_event = []
        for vote in parsed_data["votes"]:
            if vote["question"] not in questions:
                questions.add(vote["question"])
                voter = []

                if type(vote["ayes"]) is list:
                    for name in vote["ayes"]:
                        person = {
                            "name": name,
                            "response": "Yea"
                        }
                        voter.append(person)

                    for name in vote["noes"]:
                        person = {
                            "name": name,
                            "response": "Nay"
                        }

                        voter.append(person)

                    for name in vote["absent_or_not_voting"]:
                        person = {
                            "name": name,
                            "response": "NV"
                        }

                        voter.append(person)


                    event = {
                        "description": vote["question"],
                        "yeas": len(vote["ayes"]),
                        "nays": len(vote["noes"]),
                        "other": len(vote["absent_or_not_voting"]),
                        "roll_call": voter
                    }

                    voting_event.append(event)
                else:
                    event = {
                        "description": vote["question"],
                        "yeas": "unanimous",
                        "nays": 0,
                        "other": 0,
                        "roll_call": "unanimous"
                    }

                    voting_event.append(event)

        return voting_event

    except json.JSONDecodeError:
        print("Still not valid JSON. Raw output:", cleaned_response)

if __name__ == "__main__":
    test_uuid = "WI2008AB295"
    test_state = "WI"
    test_bill_id = "AB295"
    test_session = "2007"

    scrape_bill(test_uuid, test_state, test_bill_id, test_session)
