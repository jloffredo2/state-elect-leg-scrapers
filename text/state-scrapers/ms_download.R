##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Download MS Text
## Date: June 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(rvest)
library(glue)
library(fs)
library(furrr)

plan(multisession, workers = 11)

## Build list of pdf links
retrieve_pdf_links <- function(session){
  message(session)
  if(session %in% c(2008:2014)){
    url <- glue("https://billstatus.ls.state.ms.us/{session}/pdf/all_measures/allmsrs.xml")
    
    text_links <- read_html(url) |>
      html_nodes("measurelink") |>
      html_text() |>
      str_subset("pdf") |>
      str_remove("../../../")
    
  } else{
    url <- glue("https://billstatus.ls.state.ms.us/{session}/pdf/all_measures/allmsrs.htm")
    text_links <- read_html(url) |>
      html_nodes("a") |>
      html_attr("href") |>
      str_subset("pdf") |>
      str_remove("../../../")
  }
  
  text_links <- glue("https://billstatus.ls.state.ms.us/{text_links}")
  
  return(tibble(session_id = session, bill_url = text_links))
}

pdf_links <- map_dfr(2001:2014, retrieve_pdf_links) |>
  mutate(bill_id = basename(bill_url) |> str_sub(1,6))

download_text <- function(UUID, session, bill_number){
  message(UUID)
  
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/mississippi'
  
  text_link <- pdf_links |> 
    filter(session_id == session, bill_id == bill_number) |> 
    pull(bill_url)
  
  if(!is_empty(text_link)){
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    file_name <- basename(text_link)
    dest_file_path <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}")
    
    download.file(text_link, destfile = dest_file_path, mode = "wb")
  } else {
    message(glue("No text link found for {UUID} - {session} - {bill_number}"))
    return(NULL)
  }
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'MS'  & (YEAR %in% c(1995:2010) | YEAR %in% c(2011:2014) & is.na(ls_bill_id))) |>
  mutate(
    bill_id = str_remove_all(UUID, "MS"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = str_pad(bill_number, width = 4, side = "left", pad = "0"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  filter(bill_type != "NULL") |>
  select(UUID, session = year, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/mississippi"))) |>
  future_pmap(download_text, .progress = T)

bill_text_files <- data.frame(
  file_path = list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/mississippi", pattern = "*.pdf", full.names = TRUE, recursive = TRUE)) |>
  mutate(
    file_name = basename(file_path),
    UUID = str_remove(file_path, file_name) |> basename()) |>
  select(UUID, file_path) |>
  write_csv("text/state-scrapers/ms_bill_text_files.csv")
