##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape NH Bill Text
## Date: June 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(rvest)
library(glue)
library(furrr)
library(fs)

plan(multisession, workers = 11)

clean_html <- function(html_content) {
  library(stringr)
  
  cleaned <- html_content |>
    # 1. Replace bracketed strike blocks (deletions)
    str_replace_all("(?is)\\[(\\s*<s>.*?</s>\\s*)+\\]", function(x) {
      content <- str_remove_all(x, "^\\[|\\]$") |> str_squish()
      paste0("<strike class=\"amendmentDeletedText\">", content, "</strike>")
    }) |>
    
    # 2. Replace bold italics (insertions) with <u>
    str_replace_all("(?is)<b><i>(.*?)</i></b>", "<u class=\"amendmentInsertedText\">\\1</u>") |>
    
    # 3. Remove all other tags except allowed formatting
    str_replace_all("(?i)<(?!/?(u|strike)(\\s|>))[^>]+>", "") |>
    
    # 4. Remove boilerplate text and line numbers
    str_remove_all("(?i)(Explanation: Matter added.*?|Matter removed.*?|STATE OF NEW HAMPSHIRE|In the Year of Our Lord.*?|Fiscal Impact:|Methodology:)") |>
    str_remove_all("(?i)<title>.*?</title>") |>
    str_remove_all("(?i)<meta[^>]+>") |>
    str_remove_all("(?is)<!DOCTYPE.*?>") |>
    str_remove_all("-{10,}") |>
    str_remove_all("\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b") |>
    str_remove_all("\\d{1,2}[A-Za-z]{3}\\d{4}") |>  # e.g. 6Feb2014
    
    # 5. Normalize encoding and whitespace
    str_replace_all("&nbsp;|&#xA0;", " ") |>
    str_replace_all("&#167;", "§") |>
    str_replace_all("&amp;", "&") |>
    str_squish() |>
    str_trim()
  
  return(cleaned)
}

build_url <- function(session, bill_number){
  glue("https://gc.nh.gov/legislation/{session}/{bill_number}.html")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_hampshire'
  
  url <- build_url(session, bill_number)
  
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  
  if(response$status_code != 200){
    message(url)
    message(glue("Failed to fetch {UUID} - status code: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    page <- read_html(response)
    
    text <- page |> as.character() |> clean_html() |> str_trim() |> str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'NH' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "NH"),
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
  select(UUID, session = year, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_hampshire"))) |>
  future_pmap(scrape_text, .progress = T)
