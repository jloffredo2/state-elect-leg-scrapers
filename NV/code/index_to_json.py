import requests
from bs4 import BeautifulSoup
import os
import re
import json  # add JSON support
from tqdm import tqdm



def bill_index_to_json(script_dir):
    """
    This function scrapes bill information from the Nevada Legislature website and saves it to a CSV file.
    It processes multiple sessions of the legislature, extracting bill names, links, and preceding text.
    """

    # List of websites to process.
    # Each dictionary should include the URL to process and a nickname for the CSV output.
    websites = [
        {
            "url": "https://www.leg.state.nv.us/Session/70th1999/Reports/BillIndex.html",
            "nickname": "70th"
        },
        {
        "url": "https://www.leg.state.nv.us/Session/71st2001/Reports/BillIndex.htm",
            "nickname": "71st"
        },
        {
        "url": "https://www.leg.state.nv.us/Session/72nd2003/Reports/TablesAndIndex/BillIndex.html",
            "nickname": "72st"
        },
        {
        "url": "https://www.leg.state.nv.us/Session/73rd2005/Reports/TablesAndIndex/index.html",
            "nickname": "73st"
        },
        {
        "url": "https://www.leg.state.nv.us/Session/74th2007/Reports/TablesAndIndex/index.html",
            "nickname": "74st"
        },
    ]

    # Process each website in the list.
    for site in tqdm(websites, desc="Indexing sessions"):
        bill_data = {}  # Reset data for each site.
        url = site["url"]
        filenickname = site["nickname"]

        # Retrieve the HTML from the website.
        response = requests.get(url)
        response.raise_for_status()  # Raise an exception if there is any problem with the fetch.

        soup = BeautifulSoup(response.text, "html.parser")
        # for 72nd session, fetch AJR and SJR listing pages
        if filenickname == '72st':
            base_reports_url = "https://www.leg.state.nv.us/Session/72nd2003/Reports/"
            aj_list_url = base_reports_url + "HistListBills.cfm?DoctypeID=6"
            sj_list_url = base_reports_url + "HistListBills.cfm?DoctypeID=8"
            aj_list_soup = BeautifulSoup(requests.get(aj_list_url).text, 'html.parser')
            sj_list_soup = BeautifulSoup(requests.get(sj_list_url).text, 'html.parser')
        #with open(f'{site["nickname"]}.html', 'w', encoding='utf-8') as f:
        #    f.write(soup.prettify())
        # if there's no <a name="BIELECTIONS"> or <a name="ELECTIONS">, start processing right away
        if not (soup.find("a", attrs={"name": "BIELECTIONS"}) or soup.find("a", attrs={"name": "ELECTIONS"})):
            processing = True
        else:
            processing = False

        # Process each <p> tag.
        for p_tag in soup.find_all("p"):
            # Check for the starting marker if not already processing.
            if not processing:
                if p_tag.get("class") and "Level0" in p_tag.get("class"):
                    # look for either BIELECTIONS or ELECTIONS anchor to start
                    if p_tag.find("a", attrs={"name": "BIELECTIONS"}) or p_tag.find("a", attrs={"name": "ELECTIONS"}):
                        processing = True
                continue

            # Stop processing when hitting the next Level0 <p> that is not our marker.
            if p_tag.get("class") and "Level0" in p_tag.get("class"):
                # break when Level0 tag lacks both anchors
                if not (p_tag.find("a", attrs={"name": "BIELECTIONS"}) or p_tag.find("a", attrs={"name": "ELECTIONS"})):
                    break

            # Extract text that appears before the first <a> tag.
            preceding_text = ""
            for content in p_tag.contents:
                if getattr(content, "name", None) == "a":
                    break
                if isinstance(content, str):
                    preceding_text += content.strip()
            preceding_text = preceding_text.rstrip(',').replace('\n', ' ')

            # Process all <a> tags within this <p> tag.
            for a_tag in p_tag.find_all("a"):
                link = a_tag.get("href")
                raw = a_tag.get_text(strip=True)
                bill_name_raw = ''.join(raw.split())
                if filenickname == '72st':
                    # extract prefix and number
                    m = re.match(r'^(AJR|SJR|AB|SB)(\d+)', bill_name_raw)
                    if not m:
                        continue
                    prefix, number = m.groups()
                    bill_name = f"{prefix}{number}"
                    # AJR/SJR: look up specific history ID from list pages
                    if prefix in ('AJR', 'SJR'):
                        list_soup = aj_list_soup if prefix == 'AJR' else sj_list_soup
                        anchor = list_soup.find('a', string=bill_name)
                        if not anchor:
                            continue
                        href = anchor.get('href')
                        full_link = base_reports_url + href.lstrip('/')
                    else:
                        # AB/SB: use DocumentType & BillNo
                        docType = '1' if prefix == 'AB' else '2'
                        full_link = (
                            f"{base_reports_url}history.cfm?DocumentType={docType}&BillNo={number}"
                        )
                else:
                    # clean names like 'SJR5ofthe72ndSession' to just 'SJR5'
                    m2 = re.match(r'^(AJR|SJR|AB|SB)(\d+)', bill_name_raw)
                    bill_name = f"{m2.group(1)}{m2.group(2)}" if m2 else bill_name_raw
                    full_link = "https://www.leg.state.nv.us/" + link.lstrip('/')

                # skip links without 'Session'
                if "Session" not in full_link:
                    continue

                if bill_name in bill_data:
                    existing_link, existing_text = bill_data[bill_name]
                    if preceding_text and preceding_text not in existing_text:
                        aggregated_text = existing_text + "; " + preceding_text
                        bill_data[bill_name] = (existing_link, aggregated_text)
                else:
                    bill_data[bill_name] = (full_link, preceding_text)

        # Write the result to a JSON file.
        data_list = [
            {"Bill Name": bill, "Link": link, "All Preceding Texts": text}
            for bill, (link, text) in bill_data.items()
        ]
        json_filename = os.path.join(script_dir, f'bills_{filenickname}.json')
        with open(json_filename, 'w', encoding='utf-8') as jsonfile:
            json.dump(data_list, jsonfile, indent=2)
        print(f"JSON file '{json_filename}' has been created for {url}.")

