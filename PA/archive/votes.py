import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin

HEADERS = {'User-Agent': 'Mozilla/5.0'}

def fetch_legacy_bill_page(year: int, bill_number: str, body: str='H') -> str:
    session = requests.Session()
    session.headers.update(HEADERS)

    session.get('https://www.legis.state.pa.us/?legacy=1').raise_for_status()

    base = 'https://www.legis.state.pa.us/cfdocs/billInfo/billInfo.cfm'
    params = {
        'sYear': year,
        'sInd':  '0',
        'body':  body,       # 'H' or 'S'
        'type':  'B',        # bill
        'bn':    bill_number,
    }
    resp = session.get(base, params=params)
    resp.raise_for_status()

    soup = BeautifulSoup(resp.text, 'html.parser')
    frame = soup.find('frame', {'name': 'mainFrame'})
    if not frame or not frame.get('src'):
        raise RuntimeError("Could not locate mainFrame in legacy site response")

    main_url  = urljoin(resp.url, frame['src'])
    main_resp = session.get(main_url)
    main_resp.raise_for_status()
    return main_resp.text

def get_votes(year: int, bill_number: str, body: str='H') -> list[list[str]]:
    html = fetch_legacy_bill_page(year, bill_number, body)
    soup = BeautifulSoup(html, 'html.parser')

    table = soup.find('table', {'border': '1'})
    if not table:
        return []

    rows = []
    for tr in table.find_all('tr'):
        cells = [td.get_text(strip=True) for td in tr.find_all(['th','td'])]
        if cells:
            rows.append(cells)
    return rows

if __name__ == "__main__":
    votes = get_votes(1995, '2601', body='H')
    if not votes:
        print("No roll-call votes found for HB2601 (1995).")
    else:
        for row in votes:
            print(row)
