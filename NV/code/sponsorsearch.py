import requests
from bs4 import BeautifulSoup
import json
import os
import urllib.parse
from tqdm import tqdm

HEADERS = {"User-Agent": "Mozilla/5.0"}

def get_sponsor_list(search_url, form_action):
    resp = requests.get(search_url, headers=HEADERS)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "html.parser")

    selects = soup.select(f"form[action='{form_action}'] select[name='SponsorID']")
    sponsors = []
    for idx, select in enumerate(selects):
        position = "Senate" if idx == 0 else "Assembly"
        for opt in select.find_all("option"):
            sid = opt.get("value", "").strip()
            if not sid:
                continue
            name_node = opt.find(text=True, recursive=False)
            name = name_node.strip() if name_node else ""
            sponsors.append({
                "Name":      name,
                "sponsorID": sid,
                "Position":  position
            })
    return sponsors

def fetch_bills_for(sponsor_id, results_url):
    resp = requests.post(results_url, headers=HEADERS, data={"SponsorID": sponsor_id})
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "html.parser")

    bills = []
    tbl = soup.find("table", {"cellspacing": "2"})
    if tbl:
        for a in tbl.find_all("a", href=lambda h: h and h.startswith("history.cfm?ID=")):
            bills.append(a.text.strip())
    return bills

def scrape_session(session_code, input_dir, output_dir):
    """
    session_code: e.g. "70th1999"
    input_dir:    not used here, but could point to basedata if you ever want to join
    output_dir:   base directory under which we’ll write `primary/` and `secondary/`
    """
    base = f"https://www.leg.state.nv.us/Session/{session_code}/Reports/"

    # Primary sponsors
    prim_search  = urllib.parse.urljoin(base, "PrimeSponsorSearch.cfm")
    prim_results = urllib.parse.urljoin(base, "PrimeSponsorResults.cfm")
    primary = get_sponsor_list(prim_search, "PrimeSponsorResults.cfm")
    for s in primary:
        s["Billnames"] = fetch_bills_for(s["sponsorID"], prim_results)

    # Build a map of sponsorID → set(primary bills)
    primary_map = {s["sponsorID"]: set(s["Billnames"]) for s in primary}

    # Secondary sponsors
    sec_search  = urllib.parse.urljoin(base, "SponsorSearch.cfm")
    sec_results = urllib.parse.urljoin(base, "SponsorResults.cfm")
    secondary = get_sponsor_list(sec_search, "SponsorResults.cfm")
    for s in secondary:
        all_bills = fetch_bills_for(s["sponsorID"], sec_results)
        # drop any that also appear as primary
        prim_bills = primary_map.get(s["sponsorID"], set())
        s["Billnames"] = [b for b in all_bills if b not in prim_bills]

    # Write out JSON under output_dir/{primary,secondary}/{session_code}.json
    for kind, data in (("primary", primary), ("secondary", secondary)):
        kind_dir = os.path.join(output_dir, kind)
        os.makedirs(kind_dir, exist_ok=True)

        out_path = os.path.join(kind_dir, f"{session_code}.json")
        with open(out_path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2, ensure_ascii=False)

        print(f"[{session_code}][{kind}] Wrote {len(data)} sponsors → {out_path}")

def sponsor_search(input_dir, output_dir):
    sessions = ["70th1999", "71st2001", "72nd2003", "73rd2005", "74th2007"]
    for sess in tqdm(sessions, desc="Sponsor search"):
        scrape_session(sess, input_dir, output_dir)

if __name__ == "__main__":
    sponsor_search(input_dir="basedata", output_dir="sponsors")
