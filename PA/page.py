import requests
from bs4 import BeautifulSoup

url = 'https://www.palegis.us/legislation/bills/1995/sb5'
headers = {'User-Agent': 'Mozilla/5.0'}

resp = requests.get(url, headers=headers)
resp.raise_for_status()

soup = BeautifulSoup(resp.text, 'html.parser')
text = soup.get_text('\n')
lines = [line.strip() for line in text.splitlines() if line.strip()]

# find both "Sponsors" lists
try:
    idx = lines.index("Sponsors")
    sponsor_line = lines[idx + 1]
    sponsors = [name.strip().title() for name in sponsor_line.split(',')]
except ValueError:
    sponsors = []

print("Sponsors:", sponsors)


def get_bill_status(year, bill_id):
    url  = f'https://www.palegis.us/legislation/bills/{year}/{bill_id}'
    resp = requests.get(url, headers={'User-Agent':'Mozilla/5.0'})
    resp.raise_for_status()
    soup  = BeautifulSoup(resp.text, 'html.parser')

    text  = soup.get_text('\n')
    lines = [l.strip() for l in text.splitlines() if l.strip()]

    act_idx = lines.index("Actions")
    actions = []

    for line in lines[act_idx+1:]:
        if line.startswith("Generated"):
            break
        actions.append(line)

    return actions

status_actions = get_bill_status(1995, 'hb2601')
print(status_actions)
