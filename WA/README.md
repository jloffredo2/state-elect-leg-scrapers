# WA Election Bill Scrapers

## How to use Bill Scraper

To scrape a bill, create an instance of the class, call the method scrape_bill.

```
scrape_bill(uuid, state, state_bill_id, session)
```

Expected input for scrape_bill

- uuid: (str) (e.g. "WA1995H2020")
- state: (str) state abbreviation (in this case "WA")
- state_bill_id: (str) bill id including chamber (e.g. "HB2020")
- session: (str) first year of the session biennium (e.g. "1995"); this is needed for fetching session_id

## Output files

Each category (bill_history, bill_metadata, sponsors, votes) have their own directory, as listed below. The file name is formatted by {uuid}.json, besides votes where each voting has its own file with the date attached to uuid.

Output Directories

```
\output\bill_history
\output\bill_metadata
\output\sponsors
\output\votes
```
