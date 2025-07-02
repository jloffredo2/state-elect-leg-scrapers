##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape PA Bill Text
## Date: June 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(rvest)
library(polite)
library(glue)
library(fs)
library(httr)
library(furrr)

plan(multisession, workers = 4)

build_url <- function(session, bill_number){
  glue("https://www.palegis.us/legislation/bills/{session}/{bill_number}")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/pennsylvania'
  
  url <- build_url(session, bill_number)
  page <- bow(url) |> scrape()
  
  if (is_empty(page)) {
    message(url)
    message(glue("Failed to fetch {UUID}"))
    return(NULL)
  }
  
  text_link <- page |> html_nodes(".p-3") |> html_attr("href") |> str_subset("text/PDF") |> head(1)
  
  if (length(text_link) == 0) {
    message(glue("No PDF link found for {UUID}"))
    return(NULL)
  }
  
  text_link <- glue("https://www.palegis.us{text_link}")
  file_dir <- glue("{TEXT_OUTPUT_PATH}/{UUID}")
  file_name <- glue("{file_dir}/{UUID}.pdf")
  
  dir_create(file_dir)
  
  resp <- GET(
    url = text_link,
    write_disk(file_name, overwrite = TRUE),
    user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36")
  )
  
  if (resp$status_code != 200) {
    message(glue("Failed download for {UUID}, status code {resp$status_code}"))
  }
  
  Sys.sleep(5)
}

gs_pa_list <- googlesheets4::read_sheet('1riJDT0wamZmwGIl-S2fmVKv3aNN9tpx7D7bPhfQYM1s') |> janitor::clean_names()
gs_pa_list <- gs_pa_list |> 
  mutate(
    session = as.character(session),
    session = str_extract(session, "^[0-9]{4}"),
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number = str_extract(bill_number, "[0-9]+$"),
    bill_type_uuid = case_match(
      bill_type,
      "SB" ~ "S",
      "HB" ~ "H",
      .default = bill_type
    ),
    bill_id = glue("{bill_type}{bill_number}") |> str_to_lower(),,
    UUID = glue("PA{session}{bill_type_uuid}{bill_number}")
  ) |>
  select(UUID, session, bill_id) |>
  rename(bill_number = bill_id)

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'PA'  & (YEAR %in% c(1995:2010) | YEAR %in% c(2011:2014) & is.na(ls_bill_id))) |>
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
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      .default = bill_type
    ) |> str_to_lower(),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  select(UUID, session, bill_number)

master <- bind_rows(gs_pa_list, master) |> distinct()

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/pennsylvania"))) |>
  future_pmap(scrape_text, .progress = T)

bill_text_files <- data.frame(
  file_path = list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/pennsylvania", pattern = "*.pdf", full.names = TRUE, recursive = TRUE)) |>
  mutate(
    file_name = basename(file_path),
    UUID = str_remove(file_path, file_name) |> basename()) |>
  select(UUID, file_path) |>
  write_csv("text/state-scrapers/pa_bill_text_files.csv")
