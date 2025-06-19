#!/usr/bin/env python3
import os
import glob
import json
import re
import requests
from io import BytesIO
from bs4 import BeautifulSoup
from bs4.element import Tag
from tqdm import tqdm

import logging

# silence all pdfminer warnings (including the CropBox warning)
logging.getLogger("pdfminer").setLevel(logging.ERROR)
# if you want to be extra‐thorough, also silence pdfplumber’s own logger
logging.getLogger("pdfplumber").setLevel(logging.ERROR)


import pdfplumber

# ──────────────────────────────────────────────────────────────────────────────
# Configuration: URLs for summaries and statuses
# ──────────────────────────────────────────────────────────────────────────────

SESSION_SUMMARY_URLS = {
    "69th1997": {"AB": "https://www.leg.state.nv.us/Session/69th1997/tracking/abResults.cfm",
                  "SB": "https://www.leg.state.nv.us/Session/69th1997/tracking/sbResults.cfm"},
    "70th1999": "https://www.leg.state.nv.us/Session/70th1999/bdr.htm",
    "71st2001": {"A": "https://www.leg.state.nv.us/Session/71st2001/Reports/LastHistActionA.cfm",
                  "S": "https://www.leg.state.nv.us/Session/71st2001/Reports/LastHistActionS.cfm"},
    "72nd2003": "https://www.leg.state.nv.us/Session/72nd2003/BDRList/fullBDR.cfm",
    "73rd2005": "https://www.leg.state.nv.us/Session/73rd2005/BDRList/page.cfm?showAll=1",
    "74th2007": "https://www.leg.state.nv.us/Session/74th2007/BDRList/page.cfm?showAll=1",
}

SESSION_STATUS_URLS = {
    "70th1999": ("https://www.leg.state.nv.us/Session/70th1999/Reports/ABStatus.cfm",
                  "https://www.leg.state.nv.us/Session/70th1999/Reports/SBStatus.cfm"),
    "71st2001": ("https://www.leg.state.nv.us/Session/71st2001/Reports/ABStatus.cfm",
                  "https://www.leg.state.nv.us/Session/71st2001/Reports/SBStatus.cfm"),
    "72nd2003": ("https://www.leg.state.nv.us/Session/72nd2003/Reports/LastHistActionA.cfm",
                  "https://www.leg.state.nv.us/Session/72nd2003/Reports/LastHistActionS.cfm"),
    "73rd2005": ("https://www.leg.state.nv.us/Session/73rd2005/Reports/LastHistActionA.cfm",
                  "https://www.leg.state.nv.us/Session/73rd2005/Reports/LastHistActionS.cfm"),
    "74th2007": ("https://www.leg.state.nv.us/Session/74th2007/Reports/LastHistActionA.cfm",
                  "https://www.leg.state.nv.us/Session/74th2007/Reports/LastHistActionS.cfm"),
}

# ──────────────────────────────────────────────────────────────────────────────
# Fetch & cache summary soups
# ──────────────────────────────────────────────────────────────────────────────
session_soups = {}
for sess, cfg in SESSION_SUMMARY_URLS.items():
    if isinstance(cfg, dict):
        for prefix, url in cfg.items():
            r = requests.get(url); r.raise_for_status()
            session_soups[(sess, prefix)] = BeautifulSoup(r.text, 'html.parser')
    else:
        r = requests.get(cfg); r.raise_for_status()
        session_soups[(sess, None)] = BeautifulSoup(r.text, 'html.parser')

# ──────────────────────────────────────────────────────────────────────────────
# Summary parsing functions (unchanged)
# ──────────────────────────────────────────────────────────────────────────────
def parse_summary_69th(soup, bill_id):
    for tr in soup.select('table tr'):
        a = tr.find('a', href=True)
        if not a: continue
        name = a.get_text(strip=True).split('_',1)[0]
        if name.upper() != bill_id.upper(): continue
        summary_tr = tr.find_next_sibling('tr')
        raw = summary_tr.find('i') if summary_tr else None
        text = raw.get_text(' ', strip=True) if raw else ''
        return text.split('(BDR',1)[0].rstrip() if '(BDR' in text else text
    return ''

# ──────────────────────────────────────────────────────────────────────────────
# 70th Session helpers (new)
# ──────────────────────────────────────────────────────────────────────────────
_70TH_DOCTYPES = {
    'AB':1, 'SB':2, 'AR':3, 'SR':4,
    'ACR':5, 'AJR':6, 'SCR':7, 'SJR':8
}
_70TH_SOUPS = {}

def parse_summary_70th(bill_id: str) -> str:
    ###
    # 70th (1999) Session summary extractor via the HistListBills pages.
    # Picks the right Doctype page based on the bill prefix.
    # Finds the <li> whose <a href="history.cfm?..."> text matches bill_id.
    # Grabs the <i>…</i> inside that <li>, strips off trailing "(BDR…)".
    ###
    # figure out whether to look in Doctype=1,2,3…8
    # three-letter prefixes first (ACR, AJR, SCR, SJR), otherwise two-letter.
    prefix = bill_id[:3] if bill_id[:3] in _70TH_DOCTYPES else bill_id[:2]
    dt = _70TH_DOCTYPES.get(prefix.upper())
    if not dt:
        return ''

    # fetch & cache the list page
    if dt not in _70TH_SOUPS:
        url = (
            f"https://www.leg.state.nv.us"
            f"/Session/70th1999/Reports/HistListBills.cfm?Doctype={dt}"
        )
        r = requests.get(url)
        r.raise_for_status()
        _70TH_SOUPS[dt] = BeautifulSoup(r.text, 'html.parser')
    soup = _70TH_SOUPS[dt]

    # look through every <li> for our history link + description <i>
    for li in soup.find_all('li'):
        a = li.find('a', href=re.compile(r'history\.cfm\?ID='))
        if not a or a.get_text(strip=True).upper() != bill_id.upper():
            continue
        i = li.find('i')
        if not i:
            return ''
        text = i.get_text(' ', strip=True)
        # drop any trailing "(BDR…)"
        if '(BDR' in text:
            text = text.split('(BDR',1)[0].rstrip()
        return text
    return ''
def parse_summary_71st(soup, bill_id):
    for a in soup.find_all('a', href=True):
        if a.get_text(strip=True).upper() != bill_id.upper(): continue
        tr = a.find_parent('tr'); tds = tr.find_all('td') if tr else []
        if len(tds)>=4:
            text = tds[3].get_text(' ',strip=True)
            return text.split('(BDR',1)[0].rstrip() if '(BDR' in text else text
    return ''

def parse_summary_72nd(soup, bill_id):
    for a in soup.find_all('a', href=True):
        if a.get_text(strip=True).upper() != bill_id.upper(): continue
        tr = a.find_parent('tr'); tds = tr.find_all('td') if tr else []
        if len(tds)>=3:
            td = tds[2]; b = td.find('b')
            if b: b.decompose()
            text = td.get_text(' ',strip=True)
            return text.split('(BDR',1)[0].rstrip(' .') if '(BDR' in text else text
    return ''

def parse_summary_73rd(soup, bill_id):
    for a in soup.find_all('a', href=True):
        if a.get_text(strip=True).upper() != bill_id.upper(): continue
        tr = a.find_parent('tr'); tds = tr.find_all('td', recursive=False)
        if len(tds)>=3:
            td = tds[2]; br=td.find('br')
            if br:
                node=br.next_sibling
                while node and isinstance(node,str) and not node.strip(): node=node.next_sibling
                text = node.strip() if isinstance(node,str) else node.get_text(' ',strip=True)
            else:
                fst=td.find('strong')
                if fst: fst.decompose()
                text=td.get_text(' ',strip=True)
            return text.split('(BDR',1)[0].rstrip(' .') if '(BDR' in text else text
    return ''

def parse_summary_74th(soup, bill_id):
    for a in soup.find_all('a', href=True):
        if a.get_text(strip=True).upper() != bill_id.upper(): continue
        tr=a.find_parent('tr'); tds=tr.find_all('td',recursive=False)
        if len(tds)<3: continue
        td=tds[2]; brs=td.find_all('br')
        if brs:
            last=brs[-1]; node=last.next_sibling
            while node and isinstance(node,str) and not node.strip():
                node=node.next_sibling
            text=node.strip() if isinstance(node,str) else node.get_text(' ',strip=True)
        else:
            fst=td.find('strong')
            if fst: fst.decompose()
            text=td.get_text(' ',strip=True)
        # drop any SJR:/AJR:/SCR:/ACR: prefix
        text = re.sub(r"\b(?:SJR:|AJR:|SCR:|ACR:)", '', text)
        return text.split('(BDR',1)[0].rstrip(' .') if '(BDR' in text else text
    return ''

# ──────────────────────────────────────────────────────────────────────────────
# Status parsing (unchanged)
# ──────────────────────────────────────────────────────────────────────────────
HEADERS = {"User-Agent":"Mozilla/5.0"}

def parse_status_generic(url):
    r = requests.get(url, headers=HEADERS)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, 'html.parser')
    out = {}

    # Loop through every table on the page
    for tbl in soup.find_all('table'):
        for tr in tbl.find_all('tr'):
            a = tr.find('a', href=re.compile(r'history\.cfm\?ID='))
            if not a:
                continue
            tds = tr.find_all('td')
            if len(tds) < 2:
                continue

            bill = a.get_text(strip=True).upper()
            # remove any <font> tags from the status cell
            for f in tds[1].find_all('font'):
                f.decompose()
            status = re.sub(r"\s+", " ", tds[1].get_text(" ", strip=True))
            out[bill] = status

    return out

def parse_status_74(url):
    r=requests.get(url,headers=HEADERS); r.raise_for_status()
    soup=BeautifulSoup(r.text,'html.parser')
    table=next((tbl for tbl in soup.find_all('table')
                if tbl.find('a',href=re.compile(r'history\.cfm\?ID='))),
               None)
    out={}
    if not table: return out
    for tr in table.find_all('tr',valign='top'):
        tds=tr.find_all('td')
        if not tds or not tds[0].find('a'): continue
        bill=tds[0].a.get_text(strip=True).upper()
        chamber=tds[1].get_text(strip=True)
        txt=tds[2].get_text(' ',strip=True)
        status=(chamber+' '+txt).strip() if chamber else txt
        out[bill]=re.sub(r"\s+"," ",status)
    return out

def load_all_statuses():
    status_map = {}
    for sess, (aurl, surl) in SESSION_STATUS_URLS.items():
        if sess in ('72nd2003','73rd2005','74th2007'):
            a = parse_status_74(aurl)
            s = parse_status_74(surl)
        else:
            a = parse_status_generic(aurl)
            s = parse_status_generic(surl)

        # build a prefix→dict mapping that covers both bills and resolutions
        status_map[sess] = {
            'AB': a,
            'SB': s,
            'AJ': a,  # Assembly Joint Resolutions
            'AC': a,  # Assembly Concurrent Resolutions
            'SJ': s,  # Senate Joint Resolutions
            'SC': s,  # Senate Concurrent Resolutions
        }

    return status_map

# ──────────────────────────────────────────────────────────────────────────────
# Metadata: Versions‐and‐title extraction (updated)
# ──────────────────────────────────────────────────────────────────────────────

BASE = "https://www.leg.state.nv.us"

def find_version_links(detail_url):
    """
    Try to pull out explicit HTML/pdf “Versions:” links.
    If none found, we’ll fall back to detail_url itself.
    """
    try:
        r = requests.get(detail_url)
        r.raise_for_status()
    except:
        return '', ''

    soup = BeautifulSoup(r.text, 'html.parser')
    marker = soup.find(lambda t: t.name in ('b','strong')
                              and re.search(r'Versions:|Bill Text', t.get_text(), re.I))

    html_link = pdf_link = ''
    if marker:
        # primary path: the <td>→sibling <td> with your <a> tags
        td = marker.find_parent('td')
        if td:
            nxt = td.find_next_sibling('td')
            if nxt:
                for a in nxt.find_all('a', href=True):
                    href = a['href']
                    full = href if href.startswith('http') else BASE + href
                    if href.lower().endswith(('.htm','.html')):
                        html_link = full
                    elif href.lower().endswith('.pdf'):
                        pdf_link = full
        # fallback: scan siblings until an <hr>, picking up any <a>
        if not html_link and not pdf_link:
            for sib in marker.next_siblings:
                if isinstance(sib, Tag) and sib.name == 'hr':
                    break
                if isinstance(sib, Tag) and sib.name == 'a' and sib.has_attr('href'):
                    href = sib['href']
                    full = href if href.startswith('http') else BASE + href
                    if href.lower().endswith(('.htm','.html')) and not html_link:
                        html_link = full
                    elif href.lower().endswith('.pdf') and not pdf_link:
                        pdf_link = full

    # if nothing explicit, use the detail page itself:
    if not html_link:
        html_link = detail_url

    return html_link, pdf_link


def extract_act(html_url, pdf_url=''):
    """
    1) Try HTML first: look for 'AN ACT ... thereto.'
    2) If not found, scrape full page text and
    3)   a) look again for AN ACT ... thereto.
    4)   b) if still not, look for SCR/SJR/ACR/AJR … up to the first period.
    5) If all HTML fails and there's a PDF, repeat steps 1–4 on the PDF text.
    """
    FULL_TEXT = ""

    def try_html(url):
        nonlocal FULL_TEXT
        if not url or not url.lower().endswith(('.htm','html')):
            return None
        r = requests.get(url); r.raise_for_status()
        soup = BeautifulSoup(r.text, 'html.parser')
        # 1) immediate AN ACT tag
        tag = soup.find(lambda t: isinstance(t, Tag) and 'AN ACT' in t.get_text())
        if tag:
            full = ' '.join(tag.get_text(' ',strip=True).split())
            snippet = full[full.upper().find('AN ACT '):]
            m = re.search(r'\bthereto\.', snippet, flags=re.I)
            return snippet[:m.end()] if m else snippet
        # 2) accumulate full text for later
        FULL_TEXT = soup.get_text(' ', strip=True)
        return None

    def try_pdf(url):
        nonlocal FULL_TEXT
        if not url.lower().endswith('.pdf'):
            return None
        resp = requests.get(url); resp.raise_for_status()
        text_parts = []
        with pdfplumber.open(BytesIO(resp.content)) as pdf:
            for pg in pdf.pages:
                text_parts.append((pg.extract_text() or '').replace('\n',' '))
        FULL_TEXT = ' '.join(' '.join(text_parts).split())
        # 1) look for AN ACT … thereto.
        an = re.search(r'\bAN ACT .*?thereto\.', FULL_TEXT, flags=re.I)
        return an.group(0) if an else None

    # Try HTML version link first
    result = try_html(html_url)
    if result:
        return result

    # 3a) if we got FULL_TEXT from HTML, re-run AN ACT over it
    if FULL_TEXT:
        an = re.search(r'\bAN ACT .*?thereto\.', FULL_TEXT, flags=re.I)
        if an:
            return an.group(0)

        # 3b) then look for any of the resolution headings up to the first period
        res = re.search(
            r'\b(?:SENATE CONCURRENT RESOLUTION|SENATE JOINT RESOLUTION|'
            r'ASSEMBLY CONCURRENT RESOLUTION|ASSEMBLY JOINT RESOLUTION)—.*?\.',
            FULL_TEXT,
            flags=re.I
        )
        if res:
            return res.group(0).strip().replace('—',' ')

    # 4) fallback to PDF if HTML failed us
    result = try_pdf(pdf_url)
    if result:
        return result

    # 5) if PDF gave us text but no AN ACT, try resolution patterns on it
    if FULL_TEXT:
        res = re.search(
            r'\b(?:SENATE CONCURRENT RESOLUTION|SENATE JOINT RESOLUTION|'
            r'ASSEMBLY CONCURRENT RESOLUTION|ASSEMBLY JOINT RESOLUTION)—.*?\.',
            FULL_TEXT,
            flags=re.I
        )
        if res:
            return res.group(0).strip().replace('—',' ')

    return ''


# ──────────────────────────────────────────────────────────────────────────────
# Main pipeline
# ──────────────────────────────────────────────────────────────────────────────
def process_metadata(input_dir, output_dir):
    status_map = load_all_statuses()
    os.makedirs(output_dir, exist_ok=True)
    for jf in tqdm(glob.glob(os.path.join(input_dir,'bills_*.json')), desc="Processing metadata"):
        recs = json.load(open(jf, encoding='utf8'))
        out = []
        for r in recs:
            sess        = r.get('session','')
            orig_id     = r.get('state_bill_id','')
            bill_id     = re.sub(r'\*+$','', orig_id).upper()
            prefix      = bill_id[:2]

            # summary dispatch
            if sess=='69th1997':
                soup = session_soups.get((sess, prefix))
                summary = parse_summary_69th(soup, bill_id)
            elif sess=='70th1999':
                summary = parse_summary_70th(bill_id)
            elif sess=='71st2001':
                summary = parse_summary_71st(session_soups.get((sess,bill_id[0])), bill_id)
            elif sess=='72nd2003':
                summary = parse_summary_72nd(session_soups.get((sess,None)), bill_id)
            elif sess=='73rd2005':
                summary = parse_summary_73rd(session_soups.get((sess,None)), bill_id)
            elif sess=='74th2007':
                summary = parse_summary_74th(session_soups.get((sess,None)), bill_id)
            else:
                summary = ''

            # status override for 69th
            if sess=='69th1997':
                stat = "Approved by Governor"
            else:
                stat = status_map.get(sess,{}).get(prefix,{}).get(bill_id,'') or ''

            # override plain 'Governor' or 'Chapter ###.' statuses
            if re.match(r'^(governor|chapter\s*\d+\.?)$', stat, re.I):
                stat = 'Approved by Governor'

            # metadata / title
            detail_url    = r.get('Link','')
            html_link, pdf_link = find_version_links(detail_url)
            act_text      = extract_act(html_link, pdf_link)

            

            out.append({
                'uuid':           r.get('uuid',''),
                'state':          r.get('state',''),
                'session':        r.get('session',''),
                'state_bill_id':  bill_id,
                'state_url':      detail_url,
                'title':          act_text or 'NA',
                'description':    summary or 'NA',
                'status':         stat or 'NA'
            })

        fn = os.path.basename(jf).replace('bills_','metadata_')
        with open(os.path.join(output_dir,fn),'w',encoding='utf8') as wf:
            json.dump(out, wf, indent=2)
        print(f'Wrote → {fn}')
