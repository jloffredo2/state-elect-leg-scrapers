##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape CA Bill Text
## Date: May 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(jsonlite)
library(rvest)
library(glue)
library(furrr)
library(fs)

plan(multisession, workers = 11)

scrape_text <- function(UUID, session, url){
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/california'
  
  dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
  
  page <- read_html(url)
  
  text_links <- page |>
    html_elements("a") |>
    html_attr("href") |>
    str_subset("_bill_(?!.*(status|history)).*\\.htm[l]?$")
  
  text_links <- glue("http://leginfo.ca.gov{text_links}")
  
  lapply(text_links, function(link) {
    page <- read_html(link)
    text <- page |> html_element("pre") |> html_text2() |> str_trim() |> str_squish()
    
    file_name <- basename(link) |> str_remove(".html")
    dest_file_path <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}.txt")
    
    writeLines(text, dest_file_path)
  })
}

bills <- list.files(path = "CA/output/bill_metadata/", pattern = "*.json", full.names = TRUE) |>
  map_df(~ {
    json <- fromJSON(.x)
    # Remove NULL elements or replace with NA
    json[map_lgl(json, is.null)] <- NA
    as_tibble(json)
  })

bills |> 
  select(UUID = uuid, session, url = state_url) |>
  future_pmap(scrape_text, .progress = TRUE)
