##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape NJ Bill Text
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
library(httr)
library(jsonlite)

plan(multisession, workers = 11)

clean_html <- function(html_content) {
  html_content |>
    # Convert underlined spans to insertion tags
    str_replace_all(
      "<span[^>]*style=\"[^\"]*text-decoration:\\s*underline;?[^>]*\">(.*?)</span>",
      "<u class=\"amendmentInsertedText\">\\1</u>"
    ) |>
    
    # Replace bracketed deletions with strike markup
    str_replace_all(
      "\\[(.*?)\\]",
      "<strike class=\"amendmentDeletedText\">\\1</strike>"
    ) |>
    
    # Remove CSS rules in braces (e.g. `{ margin-top: 0px; }`)
    str_replace_all("\\{[^{}]*\\}", "") |>
    
    # Strip all tags except <u> and <strike>
    str_replace_all(
      "(?i)<(?!/?(u|strike)(\\s|>))[^>]+>",
      ""
    ) |>
    
    # Remove HTML comments
    str_replace_all("<!--.*?-->", "") |>
    
    # Unescape HTML entities
    str_replace_all("&nbsp;|&#xA0;", " ") |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("<u>", '<u class="amendmentInsertedText">') |>
    
    
    # Clean whitespace
    str_squish() |>
    str_trim()
}


build_url <- function(session, bill_number){
  glue("https://www.njleg.state.nj.us/api/billDetailHist/billText/{bill_number}/{session}")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_jersey'
  
  url <- build_url(session, bill_number)
  
  response <- GET(url, add_headers(`Accept` = "application/json"))
  
  if(response$status_code != 200){
    message(glue("Error: {response$status_code}"))
    return(NULL)
  } else{
    text_link <- content(response, as = "text", encoding = "UTF-8") |>
      fromJSON(data) |>
      bind_rows() |>
      filter(Description != 'Statement') |>
      pull(HTML_Link) |>
      tail(1)
    
    if(!is_empty(text_link)){
      text_link <- glue("https://pub.njleg.state.nj.us{text_link}")
      
      page <- read_html(text_link)
      text <- page |> 
        as.character() |> 
        clean_html() |> 
        str_trim() |> 
        str_squish()
      
      file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
      
      dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
      write_lines(text, file_name)
    }
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'NJ' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "NJ"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_when(
      year %in% c('2000','2001') ~ '2000',
      year %in% c('2002','2003') ~ '2002',
      year %in% c('2004','2005') ~ '2004',
      year %in% c('2006','2007') ~ '2006',
      year %in% c('2008','2009') ~ '2008',
      year %in% c('2010','2011') ~ '2010',
      year %in% c('2012','2013') ~ '2012',
      year %in% c('2014','2015') ~ '2014',
    ),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "AJRR" ~ "AJR",
      "SJRR" ~ "SJR",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  select(UUID, session, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_jersey"))) |>
  future_pmap(scrape_text, .progress = TRUE)

