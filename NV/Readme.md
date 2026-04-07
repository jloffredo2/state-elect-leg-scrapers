# Nevada Legislative Data Pipeline

Multi-stage Python script to scrape, parse, and combine Nevada legislative data (including bills, summaries, statuses, sponsors, history, and votes) into unified JSON outputs.

---

## Overview

This repo automates the end-to-end conversion of raw Nevada legislative webpages into four final JSON datasets:

1. **bill_metadata.json**
2. **sponsors.json**
3. **bill_history.json**
4. **votes.json**

The pipeline is organized into discrete stages:

1. **Index → Base Data**
2. **Base Data → Metadata**
3. **Sponsor Search & Sponsor Parsing**
4. **History Parsing**  
   Prefixes in history actions indicate chamber/source:
   - `A -` = Assembly
   - `S -` = Senate
   - `G -` = Governor
   - `P -` = Previous action prefix likely carried forward, though this was sometimes ambiguous in source data
5. **Votes Parsing**
6. **Combine All Outputs**
7. **Query Any State Bill**

Each step lives in `code/` and can be run independently, or all at once via the top-level launcher.

---

## ⚙️ Setup and Usage

### Requirements
- Python 3.8+

### Install dependencies
Create and activate a virtual environment, then install requirements:

```bash
python -m venv .venv
.venv\Scripts\activate
pip install -r requirements.txt
```

### Run the full pipeline
```bash
python code\start.py
```

The first full run takes about **15 minutes** and prints progress along the way.  
Subsequent bill queries are effectively instant once the output files have been generated.

> Intermediate JSONs may already be included, but if any conversion script is updated, you should rerun `start.py`.

### Query a bill

Query by state, session, and bill ID:

```bash
python code\query.py --state NV --session 70th1999 --state_bill_id AB444
```

Query by UUID only:

```bash
python code\query.py --uuid NV70th1999AB444
```

The resulting JSON will be written to:

```text
output/query/NV70th1999AB444.json
```

---

## 📁 Project Structure

```text
.
├── code/
│   ├── start.py          # Runs all scripts end-to-end
│   ├── index_to_json.py  # Scrape bill index pages → JSON
│   ├── basedata.py       # Normalize base bill JSON
│   ├── metadata.py       # Parse summaries, statuses, and AN ACT titles
│   ├── sponsorsearch.py  # Scrape sponsor / cosponsor listings
│   ├── sponsors.py       # Attach sponsors and cosponsors to bills
│   ├── history.py        # Parse bill history actions
│   ├── votes.py          # Parse roll-call votes
│   ├── combiner.py       # Stitch outputs into final JSON datasets
│   └── query.py          # Query a bill by UUID or by state/session/state_bill_id
│
├── intermediate/         # Working directory for intermediate pipeline outputs
│   ├── index_to_json/    # Raw bill-index JSON outputs
│   ├── basedata/         # Normalized base data used by downstream scripts
│   ├── metadata/         # bill_metadata_*.json outputs
│   ├── sponsors/         # Sponsor search + sponsor attachment outputs
│   ├── history/          # Parsed bill history JSONs
│   └── votes/            # Parsed vote JSONs
│
├── output/               # Final combined JSON outputs
│   ├── bill_metadata.json
│   ├── sponsors.json
│   ├── bill_history.json
│   ├── votes.json
│   └── query/            # Per-bill query results
│
├── requirements.txt
└── README.md
```
