import requests 
from bs4 import BeautifulSoup
import json
import os

# Function to write JSON to a file
def write_json(data, folder, filename):
  os.makedirs(folder, exist_ok=True)
  with open(os.path.join(folder, filename), "w") as f:
    json.dump(data, f, indent=4)

# Base function to scrape bill data
def scrape_bill(uuid, state_bill_id, session):
  # Set base URL depending on session and state
  base_url = f"https://www.ilga.gov/legislation/legisnet{session}/{state_bill_id}.html"

  # Step 1: Scrape bill metadata (description, title, etc.)
  # Step 2: Scrape sponsors
  # Step 4: Scrape votes (if applicable)
  # Organize data into JSON structure
  # Define output path
  # Save JSON files
  # Save votes (if applicable)

# Function to scrape bill metadata (title, description, status)
  # Return metadata in JSON format

# Function to scrape sponsors (example based on format)
  # Extract sponsors 

# Function to scrape bill history

# Function to scrape vote data
  # Check if the page exists
