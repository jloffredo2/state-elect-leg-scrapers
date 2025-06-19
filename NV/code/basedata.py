import os, sys, glob, csv, re, json, requests
from datetime import datetime
from bs4 import BeautifulSoup
from tqdm import tqdm

def process_basedata(input_dir, output_dir):
    """Add uuid, state, session, state_bill_id."""
    os.makedirs(output_dir, exist_ok=True)
    for json_path in tqdm(glob.glob(os.path.join(input_dir, 'bills_*.json')), desc="Processing basedata"):
        rows = []
        with open(json_path, encoding='utf8') as rf:
            data = json.load(rf)
            for row in data:
                link = row.get('HTML Link') or row['Link']
                soup = BeautifulSoup(requests.get(link).text, 'html.parser')
                bid_tag = soup.find('h1')
                state_bill_id = bid_tag.text.strip() if bid_tag else soup.title.text.strip()
                state_bill_id = state_bill_id.replace("*","")
                session = re.search(r'/Session/([^/]+)/', link)
                session = session.group(1) if session else ''
                uuid = f"NV{session}{state_bill_id}"
                rows.append({**row, 'uuid': uuid, 'state': 'NV',
                             'session': session, 'state_bill_id': state_bill_id})
        # write only selected metadata fields to JSON
        metadata = [
            {'uuid': row['uuid'], 'Link': row.get('Link'), 'state': row['state'],
             'session': row['session'], 'state_bill_id': row['state_bill_id']}
            for row in rows
        ]
        json_filename = os.path.basename(json_path)
        out_json = os.path.join(output_dir, json_filename)
        with open(out_json, 'w', encoding='utf8') as wf:
            json.dump(metadata, wf, indent=2)