##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape PA
## Date: July 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(googlesheets4)
library(glue)
library(furrr)
library(jsonlite)

plan(multisession, workers = 11)

# Read data downloads
actions <- read_csv("PA/raw_files/actions.csv")
bills <- read_csv("PA/raw_files/bills.csv")
sponsors <- read_csv("PA/raw_files/sponsors.csv")

# Old bill list
gs_pa_list <- googlesheets4::read_sheet('1riJDT0wamZmwGIl-S2fmVKv3aNN9tpx7D7bPhfQYM1s') |> janitor::clean_names()

scrape_bill <- function(UUID, leg_session, chamber, bill_type, bill_number){
  OUTPUT_PATH <- 'PA/output'
  
  # Retrieve entries from raw files
  cur_bill_info <- bills |> 
    filter(session_year == leg_session, body == chamber, type == bill_type, number == bill_number) |>
    tail(1)
  
  cur_bill_id <- cur_bill_info |> pull(bill_id)
  cur_actions <- actions |> filter(bill_id == cur_bill_id)
  cur_sponsors <- sponsors |> filter(bill_id == cur_bill_id) |>
    mutate(sponsor_type = ifelse(sequence_number == 1, 'sponsor', 'cosponsor'))
  cur_votes <- actions |> filter(bill_id == cur_bill_id, !is.na(roll_call_vote))
  
  status <- cur_actions |> tail(1) |> pull(full_action) |> str_trim() |> str_squish()
  status <- ifelse(str_detect(status, "Approved by the Governor"), glue("Enacted - {status}"), status)
  
  url <- glue("https://www.palegis.us/legislation/bills/{leg_session}/{chamber}{bill_type}{str_remove(bill_number, '^0')}") |> str_to_lower()
  # Prepare outputs
  bill_metadata_output <- list(
    uuid = UUID,
    state = 'PA',
    session = leg_session,
    state_bill_id = glue("{chamber}{bill_type}{str_remove(bill_number, '^0')}"),
    title = cur_bill_info |> pull(short_title),
    description = NA,
    status = status,
    state_url = url
  ) |>
    toJSON(auto_unbox = T, pretty = T) |> 
    writeLines(glue("{OUTPUT_PATH}/bill_metadata/{UUID}.json"))
  
  sponsors_output <- list(
    uuid = UUID,
    state = 'PA',
    session = leg_session,
    state_bill_id = glue("{chamber}{bill_type}{str_remove(bill_number, '^0')}"),
    sponsors = list(
      cur_sponsors |> 
        select(sponsor_name, sponsor_type) |> 
        pmap(~ list(sponsor_name = ..1, sponsor_type = ..2))
    )
  ) |>
    toJSON(auto_unbox = T, pretty = T) |> 
    writeLines(glue("{OUTPUT_PATH}/sponsors/{UUID}.json"))
  
  bill_history_output <- list(
    uuid = UUID,
    state = 'PA',
    session = leg_session,
    state_bill_id = glue("{chamber}{bill_type}{str_remove(bill_number, '^0')}"),
    bill_history = list(
      cur_actions |> 
        mutate(
          full_action = str_trim(full_action) |> 
            str_squish() |>
            str_remove("\\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\.\\s+\\d{1,2},\\s+\\d{4}\\s+\\[(House|Senate)\\]$"),
          action = glue("{action_chamber} - {full_action}")
        ) |>
        select(date, action) |> 
        pmap(~ list(date = ..1, action = ..2))
    )
  ) |>
    toJSON(auto_unbox = T, pretty = T) |> 
    writeLines(glue("{OUTPUT_PATH}/bill_history/{UUID}.json"))
  
  if(nrow(cur_votes) > 0){
    cur_votes <- cur_votes |>
      mutate(
        description = str_trim(full_action) |>
          str_squish() |>
          str_remove("\\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\.?\\s+\\d{1,2},\\s+\\d{4}(\\s+\\(\\d+-\\d+\\))?\\s+\\[(House|Senate)\\]$") |>
          str_squish() |>
          str_remove("[[:punct:]]$"),
        roll_call_vote = str_remove_all(roll_call_vote, "[()]")) |>
      separate(roll_call_vote, into = c("yeas", "nays"), sep = "-", convert = TRUE)
    
    for(v in 1:nrow(cur_votes)){
      votes_output <- list(
        uuid = UUID,
        state = 'PA',
        session = leg_session,
        state_bill_id = glue("{chamber}{bill_type}{str_remove(bill_number, '^0')}"),
        chamber = cur_votes[v,] |> pull(action_chamber),
        date = cur_votes[v,] |> pull(date) |> as.character(),
        description = cur_votes[v,] |> pull(description),
        yeas = cur_votes[v,] |> pull(yeas),
        nays = cur_votes[v,] |> pull(nays),
        other = NA,
        roll_call = NA
      ) |>
        toJSON(auto_unbox = T, pretty = T) |> 
        writeLines(glue("{OUTPUT_PATH}/votes/{UUID}_{v}.json"))
    }
  }
}

gs_pa_list <- gs_pa_list |> 
  mutate(
    session = as.character(session),
    session = str_extract(session, "^[0-9]{4}"),
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number = str_extract(bill_number, "[0-9]+$"),
    chamber = str_sub(bill_type, 1, 1),
    bill_type = str_remove(bill_type, "^[A-Z]"),
    bill_type_uuid = case_match(
      bill_type,
      "B" ~ "",
      .default = bill_type
    ),
    UUID = glue("PA{session}{chamber}{bill_type}{bill_number}"),
    bill_number = str_pad(bill_number, width = 4, pad = "0")
  ) |>
  select(UUID, session, chamber, bill_type, bill_number)

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'PA'  & YEAR %in% c(1995:2007)) |>
  mutate(
    bill_id = str_remove_all(UUID, "PA"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_match(
      year,
      "1995" ~ "1995",
      "1996" ~ "1995",
      "1997" ~ "1997",
      "1998" ~ "1997",
      "1999" ~ "1999",
      "2000" ~ "1999",
      "2001" ~ "2001",
      "2002" ~ "2001",
      "2003" ~ "2003",
      "2004" ~ "2003",
      "2005" ~ "2005",
      "2006" ~ "2005",
      "2007" ~ "2007",
      "2008" ~ "2007",
      "2009" ~ "2009",
      "2010" ~ "2009",
      "2011" ~ "2011",
      "2012" ~ "2011",
      "2013" ~ "2013",
      "2014" ~ "2013"
    ), 
    bill_type = str_extract(bill_id, "[A-Z]+"),
    chamber = str_sub(bill_type, 1, 1),
    bill_type = case_match(
      bill_type,
      "H" ~ "B",
      "S" ~ "B",
      "HR" ~ "R",
      "SR" ~ "R",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = str_pad(bill_number, width = 4, pad = "0")
  ) |>
  select(UUID, session, chamber, bill_type, bill_number)

master <- bind_rows(gs_pa_list, master) |> distinct() |> rename(leg_session = session)

master |>
  future_pmap(scrape_bill, .progress = TRUE)
