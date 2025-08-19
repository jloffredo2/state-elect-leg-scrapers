##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape TX Bill Data
## Date: February 2025
## Author: Joe Loffredo
##################################################

library(tidyverse)
library(jsonlite)
library(glue)
library(rvest)
library(threadr)
library(janitor)
library(furrr)

plan(multisession, workers = 11)

rm(list = ls())
gc()

# Functions ---------------------------------------------------------------
# Format bill base URL
build_url <- function(session, bill_number){
  # Transform bill number to match URL format
  bill_id <- str_remove_all(bill_number, "\\s")
  
  glue('https://capitol.texas.gov/BillLookup/History.aspx?LegSess={session}&Bill={bill_id}')
}

# Get bill metadata
get_bill_metadata <- function(UUID, session, bill_number, state_url, page, output_path){
  # Get metadata values
  title <- page |> html_elements('#cellCaptionText') |> html_text()
  description <- page |> html_elements('#cellSubjects') |> html_text() |> str_squish()
  status <- page |> html_elements('#cellLastAction') |> html_text()
  
  # Fix status
  status <- status |> str_remove_all("^[0-9]{2}/[0-9]{2}/[0-9]{4}") |> str_trim()
  status <- ifelse(str_detect(status, "Effective on"), glue("Enacted - {status}"), status)
  
  tibble(
    uuid = UUID, 
    state = 'TX', 
    session = session, 
    state_bill_id = bill_number, 
    title = title, 
    description = description, 
    status = status, 
    state_url = state_url
  ) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{output_path}/bill_metadata/{UUID}.json"))
}

# Get bill sponsors
get_bill_sponsors <- function(UUID, session, bill_number, page, output_path){
  # Function to clean and split text
  clean_split <- function(text) {
    str_split(text, "\\| ", simplify = TRUE) |> sapply(str_trim, side = "both")
  }
  
  # Scrape and clean authors, sponsors, and cosponsors
  authors <- clean_split(page |> html_elements("#cellAuthors") |> html_text())
  coauthors <- clean_split(page |> html_elements("#cellCoauthors") |> html_text())
  sponsors <- clean_split(page |> html_elements("#cellSponsors") |> html_text())
  cosponsors <- clean_split(page |> html_elements("#cellCosponsors") |> html_text())
  
  # Trim to what I care about
  sponsors <- c(authors, sponsors) |> as_vector()
  cosponsors <- c(coauthors, cosponsors) |> as_vector()
  
  if(is_null(sponsors)){
    return(NULL)
  } else{
    tibble(
      uuid = UUID, 
      state = 'TX', 
      session = session, 
      state_bill_id = bill_number, 
      sponsor_name = c(sponsors, cosponsors),
      sponsor_type = c(rep('sponsor', length(sponsors)), rep('cosponsor', length(cosponsors)))
    ) |> 
      group_by(uuid, state, session, state_bill_id) |>
      nest(sponsors = c(sponsor_name, sponsor_type)) |>
      ungroup() |>
      as.list() |>
      toJSON(pretty = T,auto_unbox = T) |> 
      writeLines(glue("{output_path}/sponsors/{UUID}.json"))
  }
}

get_bill_history <- function(UUID, session, bill_number, page, output_path){
  # Scrape bill history page
  history_table <- page |> html_node("table[frame='hsides'][rules='rows']")
  
  if (is.na(history_table) || length(history_table) == 0) {
    return(NULL)
  } else{
    bill_history <- page |> 
      html_node("table[frame='hsides'][rules='rows']") |> 
      html_table(fill = TRUE) |>
      row_to_names(row_number = 1) |>
      clean_names() |>
      mutate(
        journal_page = as.integer(journal_page),
        date = mdy(date),
        action = ifelse(
          !is.na(journal_page),
          glue("{description}: {description_2} {comment} (journal pg: {journal_page})") |> str_squish(),
          glue("{description}: {description_2}") |> str_squish()
        )
      ) |>
      select(date, action)
    
    tibble(
      uuid = UUID, 
      state = 'TX', 
      session = session, 
      state_bill_id = bill_number, 
      date = bill_history$date,
      action = bill_history$action
    ) |> 
      group_by(uuid, state, session, state_bill_id) |>
      nest(history = c(date, action)) |>
      ungroup() |>
      as.list() |>
      toJSON(pretty = T,auto_unbox = T) |> 
      writeLines(glue("{output_path}/bill_history/{UUID}.json"))
  }
}

scrape_bill <- function(UUID, session = NA, bill_number = NA, output_path = 'TX/output'){
  message(UUID)
  state_url <- build_url(session, bill_number)
  page <- read_html(state_url)
  
  get_bill_metadata(UUID, session, bill_number, state_url, page, output_path)
  get_bill_sponsors(UUID, session, bill_number, page, output_path)
  get_bill_history(UUID, session, bill_number, page, output_path)
}

# Process bill list -------------------------------------------------------
vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")

gs_list <- googlesheets4::read_sheet('1k2BT8QfSLm47n_vTf6yDoaoeq0zxDyXQ9ExGTSw7M_c') |> janitor::clean_names()
gs_list <- gs_list |> 
  mutate(
    bill_number = str_replace(bill_number, "\\s",""),
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number = str_extract(bill_number, "[0-9]+$"),
    bill_type_uuid = case_match(
      bill_type,
      "HB" ~ "H",
      "SB" ~ "S",
      .default = bill_type
    ),
    year = case_match(
      session,
      "80R" ~ "2007",
      "79R" ~ "2005",
      "78R" ~ "2003",
      "77R" ~ "2001",
      "76R" ~ "1999",
      "75R" ~ "1997",
      "74R" ~ "1995"
    ),
    UUID = glue("TX{year}{bill_type_uuid}{bill_number}"),
    bill_number = glue("{bill_type} {bill_number}")
  ) |>
  select(UUID, session, bill_number) 

tx_master <- vrleg_master_file |> 
  filter(STATE == 'TX' & YEAR %in% c(1995:2007)) |>
  mutate(
    bill_id = str_remove_all(UUID, "TX"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_match(
      year,
      "2008" ~ "80R",
      "2007" ~ "80R",
      "2006" ~ "79R",
      "2005" ~ "79R",
      "2004" ~ "78R",
      "2003" ~ "78R",
      "2002" ~ "77R",
      "2001" ~ "77R",
      "2000" ~ "76R",
      "1999" ~ "76R",
      "1998" ~ "75R",
      "1997" ~ "75R",
      "1996" ~ "74R",
      "1995" ~ "74R"
    ),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type} {bill_number}")
  ) |>
  select(UUID, session, bill_number)

bills_to_process <- bind_rows(tx_master,gs_list) |> distinct()

# Define output path
OUTPUT_PATH <- 'TX/output'

# Make sure output directories exist
dir.create(file.path(OUTPUT_PATH, "bill_metadata"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "sponsors"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "bill_history"), recursive = TRUE, showWarnings = FALSE)

# Add output_path column to the dataframe
bills_to_process <- bills_to_process |> 
  mutate(output_path = OUTPUT_PATH)

# Run the scraper with all parameters
bills_to_process |> future_pmap(scrape_bill)

