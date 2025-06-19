import os
import glob
import csv
import json
import re
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
from urllib.parse import urljoin
from datetime import datetime

# —————————————————————————————————————————————————————————————————————
# DATE_FORMATS & parse_date same as you have them
DATE_FORMATS = [
    "%m/%d/%y",
    "%b %d, %Y",
    "%B %d, %Y",
    "%b-%d-%Y",
    "%b.%d,%Y",
    "%b. %d, %Y",
]

def parse_date(txt):
    txt = txt.strip()
    for fmt in DATE_FORMATS:
        try:
            return datetime.strptime(txt, fmt).strftime("%Y-%m-%d")
        except ValueError:
            pass
    return txt  # fallback

def prefix_for(chamber_text):
    low = chamber_text.lower()
    if "assembly" in low:
        return "A - "
    if "senate"   in low:
        return "S - "
    return "H - "

# —————————————————————————————————————————————————————————————————————

def parse_votes(detail_url, session):
    votes = []
    # 1) load the bill detail page
    r = requests.get(detail_url)
    soup = BeautifulSoup(r.text, 'html.parser')
    # debug dump of outer page
    #os.makedirs('voteouter', exist_ok=True)
    #with open(f'voteouter/{os.path.basename(detail_url)}.html', 'w', encoding='utf8') as f:
    #    f.write(soup.prettify())

    # 2) find each vote link
    for a in soup.find_all('a', href=re.compile(r'BillVote\.cfm\?VoteID=')):
        href = a['href']
        vote_url = urljoin(detail_url, href)

        # parse chamber & description from link text
        parts = a.get_text(" ", strip=True).split()
        chamber_txt = parts[0]
        description = " ".join(parts[1:]).strip()
        if chamber_txt.lower().startswith('assembly'):
            chamber = 'A'
        elif chamber_txt.lower().startswith('senate'):
            chamber = 'S'
        else:
            chamber = 'H'

        # 3) fetch the vote page
        vr = requests.get(vote_url)
        vsoup = BeautifulSoup(vr.text, 'html.parser')
        #os.makedirs('voteinner', exist_ok=True)
        #with open(f'voteinner/{os.path.basename(vote_url)}.html', 'w', encoding='utf8') as f:
        #    f.write(vsoup.prettify())

        # 4) extract date from <font size="+2"> … on MM-DD
        date_iso = ''
        font_tag = vsoup.find('font', {'size': '+2'})
        if font_tag:
            header = font_tag.get_text(" ", strip=True)
            m = re.search(r'on\s+(\d{1,2})-(\d{1,2})(?:-(\d{2,4}))?', header)
            if m:
                mm, dd, yy = m.groups()
                if yy:
                    year = yy if len(yy) == 4 else '20' + yy
                else:
                    year = session[-4:] if len(session) >= 4 else ''
                if year:
                    date_iso = f"{int(year):04d}-{int(mm):02d}-{int(dd):02d}"

        # 5) locate the summary & roll-call tables
        summary_tbl = roll_tbl = None
        if font_tag:
            tables = font_tag.find_all_next('table')
            if len(tables) >= 1:
                summary_tbl = tables[0]
            if len(tables) >= 2:
                roll_tbl = tables[1]

        # 6) parse summary counts
        yeas = nays = other = total = 0
        if summary_tbl:
            tds = summary_tbl.find_all('td')
            for td in tds:
                txt = td.get_text(" ", strip=True)
                m = re.match(r'(\d+)\s+(.+)', txt)
                if not m:
                    continue
                cnt = int(m.group(1))
                cat = m.group(2).lower()
                total += cnt
                if cat.startswith('yea'):
                    yeas = cnt
                elif cat.startswith('nay'):
                    nays = cnt
                else:
                    other += cnt

        # 7) parse the roll-call listing
        roll_call = []
        if roll_tbl:
            rows = roll_tbl.find_all('tr')
            # skip header row if it has fewer than 2 columns or is empty
            for tr in rows[1:]:
                cols = tr.find_all('td')
                if len(cols) < 2:
                    continue
                name = cols[0].get_text(" ", strip=True)
                resp = cols[1].get_text(" ", strip=True).capitalize()
                roll_call.append({'name': name, 'response': resp})

        votes.append({
            'chamber':     chamber,
            'date':        date_iso,
            'description': description,
            'yeas':        yeas,
            'nays':        nays,
            'other':       other,
            'roll_call':   roll_call
        })

    return votes

def process_votes(input_dir, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    # process each input JSON file instead of CSV
    for json_path in tqdm(glob.glob(os.path.join(input_dir, 'bills_*.json')), desc="Processing votes"):
        rows_out = []
        with open(json_path, 'r', encoding='utf8') as rf:
            records = json.load(rf)
            for row in records:
                link    = row.get('Link', '').strip()
                session = row.get('session', '').strip()
                if link:
                    row['votes'] = parse_votes(link, session)
                else:
                    row['votes'] = []
                rows_out.append(row)
        # write output JSON preserving filename
        json_name = os.path.basename(json_path)
        out_path  = os.path.join(output_dir, json_name)
        with open(out_path, 'w', encoding='utf8') as wf:
            json.dump(rows_out, wf, indent=2)

# Example usage:
# process_votes("metadata", "history_with_votes")
