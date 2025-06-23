##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Download NM Text
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

plan(multisession, workers = 8)

build_url <- function(session, chamber, bill_type, bill_number) {
  glue("https://www.nmlegis.gov/Legislation/Legislation?chamber={chamber}&legType={bill_type}&legNo={bill_number}&year={session}")
}

scrape_text <- function(UUID, session, chamber, bill_type, bill_number){
  message(UUID)
  
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_mexico'
  
  url <- build_url(session, chamber, bill_type, bill_number)
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  
  if(response$status_code != 200){
    message(url)
    message(glue("Failed to fetch {UUID} - status code: {response$status_code}"))
    return(NULL)
  } else{
    page <- read_html(response)
    
    bill_number_pad <- ifelse(bill_type == 'B', str_pad(bill_number, 4, pad = '0'), str_pad(bill_number, 3, pad = '0'))
    bill_number_check <- glue("{chamber}{bill_type}{bill_number_pad}.pdf")
    
    text_links <- page |> 
      html_nodes("a") |> 
      html_attr("href") |> 
      str_subset("pdf") |>
      str_subset("Sessions") |>
      str_subset("bills|memorials|resolutions|final") |>
      str_subset(bill_number_check) |>
      unique()
    
    if(!is_empty(text_links)){
      text_links <- glue("https://www.nmlegis.gov{text_links}")
      
      dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
      lapply(text_links, function(link) {
        file_name <- glue("{basename(dirname(link))}_{basename(link)}") |> str_replace_all("%20", "_")
        
        file_path <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}")
        
        download.file(link, file_path)
      })
    }
  }
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'NM'  & (YEAR %in% c(1995:2010) | YEAR %in% c(2011:2014) & is.na(ls_bill_id))) |>
  mutate(
    bill_id = str_remove_all(UUID, "NM"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = str_remove(year, "^20"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    chamber = ifelse(str_starts(bill_type, "H"), "H", "S"),
    bill_type = case_match(
      bill_type,
      "H" ~ "B",
      "S" ~ "B",
      "HJR" ~ "JR",
      "HJRR" ~ "JR",
      "SJR" ~ "JR",
      "SJRR" ~ "JR",
      "HJM" ~ "JM",
      "SJM" ~ "JM",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$")
  ) |>
  select(UUID, session, chamber, bill_type, bill_number) |>
  distinct()

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_mexico"))) |>
  future_pmap(scrape_text, .progress = T)

# precedence 
get_rank <- function(path) {
  case_when(
    str_detect(path, "final_") ~ 2,
    TRUE ~ 1
  )
}

bill_text_files <- data.frame(
  file_path = list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_mexico", pattern = "*.pdf", full.names = TRUE, recursive = TRUE)) |>
  mutate(
    file_name = basename(file_path),
    rank = get_rank(file_name),
    UUID = str_remove(file_path, file_name) |> basename()) |>
  group_by(UUID) |>
  slice_max(order_by = rank, n = 1) |>
  ungroup() |>
  select(UUID, file_path) |>
  write_csv("text/state-scrapers/nm_bill_text_files.csv")
