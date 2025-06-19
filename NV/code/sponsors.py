import os
import re
import glob
import json
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm

# ———————————————————————————————————————————————————————————————
# utils
# ———————————————————————————————————————————————————————————————

def get_soup(url):
    resp = requests.get(url)
    resp.raise_for_status()
    return BeautifulSoup(resp.text, "html.parser")

def normalize_sponsor_list(raw_list):
    """
    If the tracker ever spits out ["A","B","and C"] instead of
    ["A, B, and C"], re-join them into a single entry.
    """
    return [", ".join(raw_list)] if len(raw_list) > 1 else raw_list

def load_sponsor_names(json_path, state_bill_id):
    if not os.path.exists(json_path):
        return []
    with open(json_path, encoding='utf8') as f:
        entries = json.load(f)
    return [
        rec['Name']
        for rec in entries
        if any(state_bill_id.upper() == bn.upper() for bn in rec.get('Billnames', []))
    ]

# ———————————————————————————————————————————————————————————————
# session‐specific config
# ———————————————————————————————————————————————————————————————

BASE = r"D:\Career\MIT\Research\BillText\sponsors"

SESSION_CFG = {
    "69th1997": {
        "handler": "track_69th",
    },
    "70th1999": {
        "primary":   os.path.join(BASE, "primary",   "70th1999.json"),
        "secondary": os.path.join(BASE, "secondary", "70th1999.json"),
        "fallback": {
            "tag":    ("b",   r"^By$"),
            "extract": lambda soup, tag: tag.next_sibling.strip().rstrip(",") if tag.next_sibling else ""
        }
    },
    "71st2001": {
        "primary":   os.path.join(BASE, "primary",   "71st2001.json"),
        "secondary": os.path.join(BASE, "secondary", "71st2001.json"),
        "fallback": {
            "tag":    ("b",   r"^By$"),
            "extract": lambda soup, tag: tag.next_sibling.strip().rstrip(",") if tag.next_sibling else ""
        }
    },
    "72nd2003": {
        "primary":   os.path.join(BASE, "primary",   "72nd2003.json"),
        "secondary": os.path.join(BASE, "secondary", "72nd2003.json"),
        "fallback": {
            "tag":    ("strong", r"^By:"),
            "extract": lambda soup, tag: (
                (n.get_text(strip=True) if (n := tag.find_next("b")) else "")
            )
        }
    },
    "73rd2005": {
        "primary":   os.path.join(BASE, "primary",   "73rd2005.json"),
        "secondary": os.path.join(BASE, "secondary", "73rd2005.json"),
        "fallback": {
            "tag":    ("strong", r"^By:"),
            "extract": lambda soup, tag: (
                (n.get_text(strip=True) if (n := tag.find_next("b")) else "")
            )
        }
    },
    "74th2007": {
        "primary":   os.path.join(BASE, "primary",   "74th2007.json"),
        "secondary": os.path.join(BASE, "secondary", "74th2007.json"),
        "fallback": {
            "tag":    ("strong", r"^By:"),
            "extract": lambda soup, tag: (
                # skip the first <strong> and grab the next one
                (lst := [s for s in tag.find_parent("td").find_all("strong") if s is not tag]) 
                and lst[0].get_text(strip=True)
            )
        }
    }
}

# ———————————————————————————————————————————————————————————————
# 69th tracking page handler
# ———————————————————————————————————————————————————————————————

def track_69th(link, state_bill_id):
    """
    Always returns one sponsor entry + one cosponsor:none
    """
    prefix = state_bill_id[:2].upper()
    if prefix not in ("AB", "SB"):
        return [
            {"sponsor_name": "none",     "sponsor_type": "sponsor"},
            {"sponsor_name": "none",     "sponsor_type": "cosponsor"},
        ]

    page = "abResults.cfm" if prefix=="AB" else "sbResults.cfm"
    url  = f"https://www.leg.state.nv.us/Session/69th1997/tracking/{page}"
    track = get_soup(url)

    sponsor_text = None
    for tr in track.select("table tr"):
        tds = tr.find_all("td")
        if len(tds)>=4 and (a:=tds[0].find("a")):
            if a.get_text(strip=True).split("_",1)[0].upper()==state_bill_id.upper():
                sponsor_text = tds[3].get_text(" ", strip=True)
                break

    # build the two-entry list
    return [
        {
            "sponsor_name": sponsor_text or "none",
            "sponsor_type": "sponsor"
        },
        {
            "sponsor_name": "none",
            "sponsor_type": "cosponsor"
        }
    ]

# ———————————————————————————————————————————————————————————————
# generic sponsor parser
# ———————————————————————————————————————————————————————————————

def parse_sponsors(session, link, state_bill_id):
    cfg = SESSION_CFG.get(session)
    if not cfg:
        return []

    # 1) 69th special
    if cfg.get("handler") == "track_69th":
        return track_69th(link, state_bill_id)

    # 2) primary
    sponsors = []
    pnames = load_sponsor_names(cfg["primary"], state_bill_id)
    if pnames:
        sponsors += [{"sponsor_name":n, "sponsor_type":"sponsor"} for n in pnames]
    else:
        # fallback: fetch bill page only now
        soup = get_soup(link)
        tagname, pat = cfg["fallback"]["tag"]
        tag = soup.find(tagname, string=re.compile(pat, re.I))
        name = cfg["fallback"]["extract"](soup, tag) if tag else ""
        sponsors.append({
            "sponsor_name": name or "none",
            "sponsor_type": "sponsor"
        })

    # 3) cosponsors
    cnames = load_sponsor_names(cfg["secondary"], state_bill_id)
    if cnames:
        sponsors += [{"sponsor_name":n, "sponsor_type":"cosponsor"} for n in cnames]
    else:
        sponsors.append({"sponsor_name":"none","sponsor_type":"cosponsor"})

    return sponsors

# ———————————————————————————————————————————————————————————————
# driver
# ———————————————————————————————————————————————————————————————

def process_sponsors(input_dir, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    for inp in tqdm(glob.glob(os.path.join(input_dir, "bills_*.json")), desc="Processing sponsors"):
        out = []
        print(f"Processing {inp}")
        with open(inp, encoding='utf8') as f:
            bills = json.load(f)
        for row in bills:
            sess = row.get("session","")
            link = row.get("Link","")
            sbid = row.get("state_bill_id","")
            #print(f"→ {sess} {sbid}")
            sponsors = parse_sponsors(sess, link, sbid)
            out.append({**row, "sponsors": sponsors})
        with open(os.path.join(output_dir, os.path.basename(inp)), "w", encoding='utf8') as f:
            json.dump(out, f, indent=2)

if __name__ == "__main__":
    process_sponsors("basedata", "sponsors")
