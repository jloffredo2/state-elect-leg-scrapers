import os, glob, csv, json, re, requests
from datetime import datetime
from bs4 import BeautifulSoup
from tqdm import tqdm

# ---------------------------------------------------
# replace your DATE_PATTERNS entirely with this:

DATE_FORMATS = [
    "%m/%d/%y",   # 04/03/97
    "%b %d, %Y",  # Apr  3, 1997
    "%B %d, %Y",  # April 3, 1997
    "%b-%d-%Y",   # Apr-21-1999
    "%b.%d,%Y",   # Apr.01,1999
    "%b. %d, %Y",   # <— new for "Mar. 27, 2007"
]
# ---------------------------------------------------

# — detect any of your possible raw date strings for the <li> backwards scan —
DATE_DETECTOR = re.compile(
    r"\d{2}/\d{2}/\d{2}"           # 04/03/97
    r"|[A-Za-z]{3}-\d{1,2}-\d{4}"  # Apr-21-1999
    r"|[A-Za-z]{3}\.\d{1,2},\d{4}" # Apr.01,1999
    r"|[A-Za-z]{3} \d{1,2}, \d{4}" # Apr  1, 1999
)


def parse_date(txt):
    txt = txt.strip()
    for fmt in DATE_FORMATS:
        try:
            return datetime.strptime(txt, fmt).strftime("%Y-%m-%d")
        except ValueError:
            pass
    # fallback: return raw so you can catch any truly unexpected format
    return txt


def prefix_for(action_text):
    low = action_text.lower()
    if "assembly" in low:
        return "A - "
    if "senate" in low:
        return "S - "
    # note: this catches everything else as Previous
    return "P - "

def parse_history_69th(soup):
    entries = []
    content = soup.find("div", id="content") or soup

    # 1) Introduced & Introduced By
    intro_date = None
    b_intro = content.find("b", string="Introduced:")
    if b_intro:
        intro_date = parse_date(b_intro.next_sibling.strip())

    b_intro_by = content.find("b", string="Introduced By:")
    if b_intro_by and intro_date:
        party = b_intro_by.next_sibling.strip()
        entries.append({
            "date": intro_date,
            "action": prefix_for("Introduced By") + f"Introduced By: {party}"
        })
    # 2) Committee hearings
    b_comm = content.find(
        "b", 
        string=re.compile(r"Heard in the the following Committees", re.I)
    )
    if b_comm and intro_date:
        # find the <table> that *contains* that <b> …
        comm_table = b_comm.find_parent("table")
        for tr in comm_table.find_all("tr")[1:]:
            tds = tr.find_all("td")
            if len(tds) < 3:
                continue
            chamber = tds[1].get_text(strip=True).rstrip(":")
            info    = tds[2].get_text(strip=True)
            # same regex as before to split off dates
            m = re.match(
                r"(.+?)\s+((?:\d{1,2}-\d{1,2})(?:;\s*\d{1,2}-\d{1,2})*)",
                info
            )
            if not m:
                continue
            comm_name, dates_str = m.groups()
            comm_name = comm_name.title()
            year = intro_date.split("-")[0]
            for piece in dates_str.split(";"):
                mm, dd = piece.strip().split("-")
                date = f"{year}-{int(mm):02d}-{int(dd):02d}"
                entries.append({
                    "date":   date,
                    "action": prefix_for(chamber)
                              + f"Heard in {chamber} Committee {comm_name}"
                })

    # 3) The main <ul> timeline
    ul = content.find("ul")
    if ul:
        for li in ul.find_all("li"):
            # grab only the direct text (so nested <li>s don't bleed in)
            direct = [
                t.strip()
                for t in li.find_all(text=True, recursive=False)
                if t.strip()
            ]
            if not direct:
                continue
            text = " ".join(direct)
            m = re.match(r"(\d{2}/\d{2}/\d{2})\s+(.*)", text)
            if not m:
                continue
            raw_date, action_txt = m.groups()
            date = parse_date(raw_date)
            pref = prefix_for(action_txt)
            action = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date, "action": action})

    entries.sort(key=lambda ev: ev["date"])
    # then re-index
    return {i+1: ev for i, ev in enumerate(entries)}


def parse_history_70_71th(soup):
    entries = []
    content = soup.find("body") or soup

    # 1) Introduced on … By sponsor
    b_intro = content.find("b", string="Introduced on")
    if b_intro:
        raw = b_intro.next_sibling.strip()
        intro_date = parse_date(raw)
        b_by = content.find("b", string="By")
        if b_by:
            sponsor = b_by.next_sibling.strip().rstrip(",")
            entries.append({
                "date": intro_date,
                "action": prefix_for("Introduced By")
                          + f"Introduced By: {sponsor}"
            })

    # 2) Hearings table (skip "No Action")
    b_hear = content.find("b", string="Hearings")
    if b_hear:
        # the second <td> next to that <b> holds the <table>
        hear_td = b_hear.parent.find_next_sibling("td")
        if hear_td:
            hear_tbl = hear_td.find("table")
            if hear_tbl:
                for tr in hear_tbl.find_all("tr"):
                    cols = tr.find_all("td")
                    if len(cols) < 3:
                        continue
                    comm      = cols[0].get_text(" ", strip=True)
                    raw_date  = cols[1].get_text(strip=True)
                    act_text  = cols[2].get_text(" ", strip=True)
                    if act_text.lower() == "no action":
                        continue
                    if not act_text.strip():
                        continue
                    date = parse_date(raw_date)
                    pref = prefix_for(comm)
                    action = act_text if act_text.startswith(pref) else pref + act_text
                    entries.append({"date": date, "action": action})

    # 3) Bottom timeline bullets (<li> outside tables)
    for li in content.find_all("li"):
        if li.find_parent("table"):
            continue
        # walk backwards to find the nearest date string
        date_str = None
        sib = li.previous_sibling
        while sib:
            txt = ""
            if isinstance(sib, str):
                txt = sib.strip()
            elif getattr(sib, "get_text", None):
                txt = sib.get_text(strip=True)
            m = DATE_DETECTOR.search(txt)
            if m:
                date_str = m.group(0)
                break
            sib = sib.previous_sibling
        if not date_str:
            continue
        date = parse_date(date_str)
        act = " ".join(li.stripped_strings)
        pref = prefix_for(act)
        action = act if act.startswith(pref) else pref + act
        entries.append({"date": date, "action": action})

    # 4) sort & re-index
    entries.sort(key=lambda e: e["date"])
    return {i+1: ev for i, ev in enumerate(entries)}


    entries = []
    content = soup.find("body") or soup

    # --- 1) Introduced on / By ---
    intro_lbl = content.find(
        "strong", string=re.compile(r"Introduced\s+on", re.I)
    )
    if intro_lbl:
        raw_date = intro_lbl.next_sibling.strip()
        date_iso = parse_date(raw_date)

        by_lbl = content.find(
            "strong", string=re.compile(r"By:", re.I)
        )
        if by_lbl:
            # skip the "(Bolded name…)" <b>, pick the first real sponsor
            sponsor = None
            for b in by_lbl.find_all_next("b"):
                txt = b.get_text(strip=True)
                if txt.lower() != "bolded":
                    sponsor = txt
                    break
            if sponsor:
                entries.append({
                    "date":   date_iso,
                    "action": prefix_for("Introduced By")
                              + f"Introduced By: {sponsor}"
                })

    # --- 2) Past Hearings table ---
    hear_lbl = content.find(
        lambda t: t.name=="strong" and "Past Hearings" in t.text
    )
    if hear_lbl:
        tbl = hear_lbl.find_parent("table")
        if tbl:
            # skip header row
            for tr in tbl.find_all("tr")[1:]:
                cols = tr.find_all("td")
                if len(cols) < 4:
                    continue
                chamber    = cols[0].get_text(strip=True)
                raw_date   = cols[1].get_text(strip=True)
                action_txt = cols[3].get_text(" ", strip=True)
                if not action_txt or action_txt.lower()=="no action":
                    continue
                date = parse_date(raw_date)
                pref = prefix_for(chamber)
                act  = action_txt if action_txt.startswith(pref) \
                       else pref + action_txt
                entries.append({"date": date, "action": act})

    # --- 3) Bill History bullets ---
    bh_lbl = content.find(
        lambda t: t.name in ("strong","b") and "Bill History" in t.text
    )
    if bh_lbl:
        # look for every JournalPopup link under that section
        for a in bh_lbl.find_all_next("a", href=lambda h: h and "JournalPopup" in h):
            date = parse_date(a.get_text(strip=True))
            # the pattern on these pages is:
            # <tr><td><p><a>DATE</a></p></td></tr>
            # <tr><td><ul><li>ACTION</li></ul></td></tr>
            tr_date   = a.find_parent("tr")
            tr_action = tr_date.find_next_sibling("tr")
            if not tr_action:
                continue
            li = tr_action.find("li")
            if not li:
                continue
            action_txt = " ".join(li.stripped_strings)
            pref = prefix_for(action_txt)
            act  = action_txt if action_txt.startswith(pref) \
                   else pref + action_txt
            entries.append({"date": date, "action": act})

    # --- 4) sort & re-index ---
    entries.sort(key=lambda ev: ev["date"])
    return {i+1: ev for i, ev in enumerate(entries)}

def parse_history_72nd(soup):
    entries = []
    content = soup.find("body") or soup

    # --- 1) Introduced on / By ---
    intro_lbl = content.find("strong", string=re.compile(r"Introduced\s+on", re.I))
    if intro_lbl:
        raw_date = intro_lbl.next_sibling.strip()
        date_iso = parse_date(raw_date)

        by_lbl = content.find("strong", string=re.compile(r"By:", re.I))
        if by_lbl:
            cell = by_lbl.parent
            full_text = cell.get_text(" ", strip=True)
            # strip the parenthetical "(Bolded …)" and split sponsors
            sponsor_text = re.sub(r"^By:\s*\([^)]*\)\s*", "", full_text)
            sponsors = [s.strip() for s in sponsor_text.split(",") if s.strip()]
            if sponsors:
                entries.append({
                    "date":   date_iso,
                    "action": prefix_for("Introduced By")
                              + "Introduced By: " + ", ".join(sponsors)
                })

    # --- 2) Past Hearings table ---
    hear_lbl = content.find(lambda t: t.name=="strong" and "Past Hearings" in t.text)
    if hear_lbl:
        tbl = hear_lbl.find_parent("table")
        for tr in tbl.find_all("tr")[1:]:  # skip header row
            tds = tr.find_all("td", recursive=False)
            if len(tds) < 3:
                continue
            chamber    = tds[0].get_text(strip=True)
            dt_txt     = tds[1].get_text(" ", strip=True)
            raw_date   = dt_txt.split()[0]           # e.g. "Mar-25-2003"
            action_txt = tds[2].get_text(" ", strip=True)
            if action_txt.lower() == "no action":
                continue
            date = parse_date(raw_date)
            pref = prefix_for(chamber)
            act  = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date, "action": act})

    # --- 3) Bill History bullets ---
    bh_lbl = content.find(lambda t: t.name in ("strong","b") and "Bill History" in t.text)
    if bh_lbl:
        for a in bh_lbl.find_all_next("a", href=lambda h: h and "JournalPopup" in h):
            date = parse_date(a.get_text(strip=True))
            tr_date   = a.find_parent("tr")
            tr_action = tr_date.find_next_sibling("tr")
            if not tr_action:
                continue
            li = tr_action.find("li")
            if not li:
                continue
            action_txt = " ".join(li.stripped_strings)
            pref = prefix_for(action_txt)
            act  = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date, "action": act})

    # --- 4) sort & re-index in chronological order ---
    entries.sort(key=lambda ev: ev["date"])
    return {i+1: ev for i, ev in enumerate(entries)}

def parse_history_73rd(soup):
    entries = []
    content = soup.find("body") or soup

    # --- 1) Introduced on / By ---
    intro_lbl = content.find("strong", string=re.compile(r"Introduced\s+on", re.I))
    if intro_lbl:
        raw_date = intro_lbl.next_sibling.strip()
        date_iso = parse_date(raw_date)

        by_lbl = content.find("strong", string=re.compile(r"By:", re.I))
        if by_lbl:
            # collect all sponsors after stripping the "(…)" note
            cell_text = by_lbl.parent.get_text(" ", strip=True)
            sponsor_text = re.sub(r"^By:\s*\([^)]*\)\s*", "", cell_text)
            sponsors = [s.strip() for s in sponsor_text.split(",") if s.strip()]
            if sponsors:
                entries.append({
                    "date":   date_iso,
                    "action": prefix_for("Introduced By")
                              + "Introduced By: " + ", ".join(sponsors)
                })

    # --- 2) Past Hearings table ---
    hear_lbl = content.find(lambda t: t.name in ("strong","b") and "Past Hearings" in t.text)
    if hear_lbl:
        tbl = hear_lbl.find_parent("table")
        # skip header row
        for tr in tbl.find_all("tr")[1:]:
            tds = tr.find_all("td", recursive=False)
            # need at least 4 columns: Chamber, Date+Time, Minutes link, Action
            if len(tds) < 4:
                continue

            chamber = tds[0].get_text(" ", strip=True)

            # extract date portion only, guard against empty cells
            dt_txt = tds[1].get_text(" ", strip=True)
            if not dt_txt:
                continue
            parts = dt_txt.split()
            if not parts:
                continue
            raw_date = parts[0]  # e.g. "May-02-2005"

            action_txt = tds[3].get_text(" ", strip=True)
            # skip "No Action." or blank actions
            if not action_txt or re.match(r"no action\.?$", action_txt, re.I):
                continue

            date = parse_date(raw_date)
            pref = prefix_for(chamber)
            act  = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date, "action": act})

    # --- 3) Bill History bullets ---
    bh_lbl = content.find(lambda t: t.name in ("strong","b") and "Bill History" in t.text)
    if bh_lbl:
        for a in bh_lbl.find_all_next("a", href=lambda h: h and "JournalPopup" in h):
            date = parse_date(a.get_text(strip=True))
            tr_date   = a.find_parent("tr")
            tr_action = tr_date.find_next_sibling("tr")
            if not tr_action:
                continue
            li = tr_action.find("li")
            if not li:
                continue
            action_txt = " ".join(li.stripped_strings)
            pref = prefix_for(action_txt)
            act  = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date, "action": act})

    # --- 4) sort chronologically & re-index 1→N ---
    entries.sort(key=lambda ev: ev["date"])
    return {i+1: ev for i, ev in enumerate(entries)}


    entries = []
    content = soup.find("body") or soup

    # 1) Introduced on / By
    intro_lbl = content.find("strong", string=re.compile(r"Introduced\s+on", re.I))
    if intro_lbl:
        raw = intro_lbl.next_sibling.strip()
        date_iso = parse_date(raw)

        by_lbl = content.find("strong", string=re.compile(r"^By:", re.I))
        if by_lbl:
            txt = by_lbl.parent.get_text(" ", strip=True)
            # remove "(Bolded…)" then split
            txt = re.sub(r"^By:\s*\([^)]*\)\s*", "", txt)
            sponsors = [s.strip() for s in txt.split(",") if s.strip()]
            if sponsors:
                entries.append({
                    "date":   date_iso,
                    "action": prefix_for("Introduced By")
                              + "Introduced By: " + ", ".join(sponsors)
                })

    # 2) Past Hearings table
    hear_lbl = content.find(lambda t: t.name in ("strong","b")
                                    and "Past Hearings" in t.text)
    if hear_lbl:
        tbl = hear_lbl.find_parent("table")
        for tr in tbl.find_all("tr")[1:]:  # skip header
            tds = tr.find_all("td", recursive=False)
            if len(tds) < 4:
                continue

            chamber = tds[0].get_text(" ", strip=True)

            # strip off trailing time (like "03:45 PM")
            dt_txt = tds[1].get_text(" ", strip=True)
            raw_date = re.sub(r"\s*\d{1,2}:\d{2}\s*[AP]M$", "", dt_txt).strip()
            date_iso = parse_date(raw_date)

            action_txt = tds[3].get_text(" ", strip=True)
            # skip any "No Action" hearing
            if action_txt.lower().startswith("no action"):
                continue

            pref = prefix_for(chamber)
            action = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date_iso, "action": action})

    # 3) Bill History bullets (unchanged)
    bh_lbl = content.find(lambda t: t.name in ("strong","b")
                                   and "Bill History" in t.text)
    if bh_lbl:
        for a in bh_lbl.find_all_next("a", href=lambda h: h and "JournalPopup" in h):
            d = parse_date(a.get_text(strip=True))
            tr_d = a.find_parent("tr")
            tr_a = tr_d.find_next_sibling("tr")
            if not tr_a:
                continue
            li = tr_a.find("li")
            if not li:
                continue
            act_txt = " ".join(li.stripped_strings)
            pref    = prefix_for(act_txt)
            act     = act_txt if act_txt.startswith(pref) else pref + act_txt
            entries.append({"date": d, "action": act})

    # 4) sort & re-index
    entries.sort(key=lambda e: e["date"])
    return {i+1: ev for i, ev in enumerate(entries)}

def parse_history_74th(soup):
    entries = []
    content = soup.find("body") or soup

    # --- 1) Introduced on / By ---
    intro_lbl = content.find("strong", string=re.compile(r"Introduced\s+on", re.I))
    if intro_lbl:
        raw = intro_lbl.next_sibling.strip()
        date_iso = parse_date(raw)

        # sponsor block uses the "(Bolded …)" note
        bold_note = content.find("strong", string=re.compile(r"Bolded", re.I))
        if bold_note:
            td = bold_note.find_parent("td")
            txt = td.get_text(" ", strip=True)
            # strip off the parenthetical, then split on commas
            sponsor_text = re.sub(r"^.*\)\s*", "", txt)
            sponsors = [s.strip() for s in sponsor_text.split(",") if s.strip()]
            if sponsors:
                entries.append({
                    "date":   date_iso,
                    "action": prefix_for("Introduced By")
                              + "Introduced By: " + ", ".join(sponsors)
                })

    # --- 2) Past Hearings table ---
    hear_lbl = content.find(lambda t: t.name in ("strong","b") and "Past Hearings" in t.text)
    if hear_lbl:
        tbl = hear_lbl.find_parent("table")
        for tr in tbl.find_all("tr")[1:]:
            tds = tr.find_all("td", recursive=False)
            if len(tds) < 4:
                continue

            chamber = tds[0].get_text(" ", strip=True)
            dt_txt  = tds[1].get_text(" ", strip=True)
            # drop the time suffix
            raw_date = re.sub(r"\s*\d{1,2}:\d{2}\s*[AP]M$", "", dt_txt).strip()
            action_txt = tds[3].get_text(" ", strip=True)
            lower_txt  = action_txt.lower()
            if not action_txt or lower_txt.startswith("no action") or "no further action" in lower_txt:
                continue


            date_iso = parse_date(raw_date)
            pref     = prefix_for(chamber)
            action   = action_txt if action_txt.startswith(pref) else pref + action_txt
            entries.append({"date": date_iso, "action": action})

    # --- 3) Bottom‐of‐page journal popup bullets (unchanged) ---
    bh_lbl = content.find(lambda t: t.name in ("strong","b") and "Bill History" in t.text)
    if bh_lbl:
        for a in bh_lbl.find_all_next("a", href=lambda h: h and "JournalPopup" in h):
            date_iso  = parse_date(a.get_text(strip=True))
            tr_date   = a.find_parent("tr")
            tr_act    = tr_date.find_next_sibling("tr")
            if not tr_act:
                continue
            li = tr_act.find("li")
            if not li:
                continue
            action_txt = " ".join(li.stripped_strings)
            pref       = prefix_for(action_txt)
            action     = action_txt if action_txt.startswith(pref) else pref + action_txt
            lower_txt  = action.lower()
            if not action_txt or lower_txt.startswith("no action") or "no further action" in lower_txt:
                continue
            entries.append({"date": date_iso, "action": action})

    # --- 4) Filter out any blank‐date or blank‐action‐only entries ---
    entries = [
        ev for ev in entries
        if ev["date"]                                              # has a date
        and ev["action"].strip()                                   # non‐empty action
        and not re.match(r"^[A-Z] -\s*$", ev["action"])            # not just "P - "
    ]

    # --- 5) Sort & re‐index chronologically ---
    entries.sort(key=lambda ev: ev["date"])
    return {i+1: ev for i, ev in enumerate(entries)}

def adjust_prefix(bill_history, bill_id):
    # choose S or A if bill_id starts with that letter
    bid = bill_id.upper()
    if bid.startswith('S'):
        base = 'S - '
    elif bid.startswith('A'):
        base = 'A - '
    else:
        return bill_history
    # find the first "Introduced By" entry index
    idxs = sorted(bill_history.keys())
    first_intro = next((i for i in idxs
                        if 'introduced by' in bill_history[i]['action'].lower()), None)
    if first_intro is None:
        return bill_history
    end = first_intro + 1  # include the "Read first time" action right after
    for i in idxs:
        if i <= end:
            # strip existing prefix like "P - ", "A - ", "S - "
            act = re.sub(r'^[A-Z]\s*-\s*', '', bill_history[i]['action'])
            bill_history[i]['action'] = base + act
    # infer chamber transitions based on “To Senate” / “To Assembly” markers
    keys = sorted(bill_history.keys(), key=int)
    governor_applied = False
    for idx in keys:
        action = bill_history[idx]['action']
        raw = re.sub(r'^[A-Z]\s*-\s*', '', action)
        low = raw.lower()

        if not governor_applied and 'governor' in low:      # first gov transition
            governor_applied = True
            # find last explicit S/A prefix before this point
            prior = [j for j in keys if j<idx and bill_history[j]['action'].startswith(('S - ','A - '))]
            anchor = max(prior) if prior else None
            # apply origin‐chamber prefix to entries up through this transition
            if anchor:
                orig = bill_history[anchor]['action'][0]
                for k in keys:
                    if anchor<k<=idx:
                        txt = re.sub(r'^[A-Z]\s*-\s*', '', bill_history[k]['action'])
                        bill_history[k]['action'] = f"{orig} - {txt}"
            # prefix all later entries G -
            for k in keys:
                if k>idx:
                    txt = re.sub(r'^[A-Z]\s*-\s*', '', bill_history[k]['action'])
                    bill_history[k]['action'] = f"G - {txt}"
            continue

        if 'to senate' in low:
            origin, dest = 'A', 'S'
        elif 'to assembly' in low:
            origin, dest = 'S', 'A'
        else:
            continue

        # existing S/A transition logic...
        prior_idxs = [j for j in keys if j<idx and bill_history[j]['action'].startswith(f"{origin} - ")]
        anchor = max(prior_idxs) if prior_idxs else 0
        for k in keys:
            if anchor<k<=idx:
                txt = re.sub(r'^[A-Z]\s*-\s*', '', bill_history[k]['action'])
                bill_history[k]['action'] = f"{origin} - {txt}"
        # after transfer, subsequent entries default to dest until changed again

    return bill_history

def process_history(input_dir, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    for json_path in tqdm(glob.glob(os.path.join(input_dir, "bills_*.json")), desc="Processing history"):
        out_rows = []
        with open(json_path, 'r', encoding='utf8') as rf:
            records = json.load(rf)
            for row in records:
                link     = row["Link"]
                session  = row["session"]
                r = requests.get(link)
                soup = BeautifulSoup(r.text, "html.parser")
                if session == "69th1997":
                    bill_history = parse_history_69th(soup)
                elif session in ("70th1999", "71st2001"):
                    bill_history = parse_history_70_71th(soup)
                elif session == "72nd2003":
                    bill_history = parse_history_72nd(soup)
                elif session == "73rd2005":
                    bill_history = parse_history_73rd(soup)
                elif session == "74th2007":
                    bill_history = parse_history_74th(soup)
                else:
                    # unknown session: skip this record
                    continue
                # adjust initial prefixes based on state_bill_id
                bill_history = adjust_prefix(bill_history, row.get('state_bill_id', ''))
                out_rows.append({**row, "bill_history": bill_history})

        # write output JSON
        json_name = os.path.basename(json_path)
        with open(os.path.join(output_dir, json_name), "w", encoding="utf8") as wf:
            json.dump(out_rows, wf, indent=2)

# example usage:
# process_history("data/input", "data/output")
