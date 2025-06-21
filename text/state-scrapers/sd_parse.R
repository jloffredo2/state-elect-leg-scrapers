##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape SD Bill Text
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

text_links <- read_csv("text/state-scrapers/sd_bill_links.csv")

clean_html <- function(html_content) {
  library(xml2)
  library(stringr)
  library(rvest)
  
  html_content <- as.character(html_content)
  doc <- read_html(html_content)
  
  # Remove comments
  xml_find_all(doc, ".//comment()") |> xml_remove()
  
  # Standardize <s> to <strike>
  xml_find_all(doc, ".//s") |> xml_set_name("strike")
  
  # Add amendment classes
  xml_find_all(doc, ".//u") |> xml_set_attr("class", "amendmentInsertedText")
  xml_find_all(doc, ".//strike") |> xml_set_attr("class", "amendmentDeletedText")
  
  # Isolate <body>
  body <- xml_find_first(doc, "//body")
  
  # Remove all other tags (flatten them)
  all_nodes <- xml_find_all(body, ".//*")
  for (node in all_nodes) {
    tag <- xml_name(node)
    cls <- xml_attr(node, "class")
    if (!((tag == "u" && cls == "amendmentInsertedText") ||
          (tag == "strike" && cls == "amendmentDeletedText"))) {
      xml_set_name(node, "span")
      xml_attrs(node) <- NULL
    }
  }
  
  # Convert to string
  html_str <- as.character(body)
  
  # Final cleanup
  cleaned <- html_str |>
    str_replace_all("(?i)<(?!/?(u|strike)(\\s|>))[^>]+>", "") |>   # remove all but allowed tags
    str_replace_all("&nbsp;|&#xA0;", " ") |>                      # unescape space
    str_replace_all("&#167;", "§") |>
    str_replace_all("&amp;", "&") |>
    str_squish() |>
    str_trim() |>
    str_replace_all("(?i)(Insertions into existing statutes.*|Legislative Research Council.*|[0-9]+ copies of this document.*)", "") |>
    str_replace_all("(?is)(I certify that the attached Act originated in the HOUSE.*)", "") |>
    str_remove("\\.\\s*\\.$") |>   # remove ". ."
    str_trim()
  
  return(cleaned)
}

get_bill_id <- function(year, bill_number){
  text_links |> filter(session == year & state_bill_number == bill_number) |> pull(state_bill_id)
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/south_dakota'
  
  bill_id <- get_bill_id(session, bill_number)
  document_versions <- glue("https://sdlegislature.gov/api/Bills/Versions/{bill_id}")
  
  response <- GET(document_versions, add_headers(`Accept` = "application/json"))
  
  if(response$status_code != 200){
    message(glue("Error: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    data <- content(response, as = "text", encoding = "UTF-8")
    document_id <- fromJSON(data, flatten = TRUE) |> pull(DocumentId) |> tail(1)
    
    document_url <- glue("https://sdlegislature.gov/api/Bills/HTML/{document_id}")
    
    text <- fromJSON(document_url) |> pluck('DocumentHtml') |> clean_html() |> str_trim() |> str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'SD' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "SD"),
    year = str_extract(bill_id, "^[0-9]{4}") |> as.numeric(),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      "SJRR" ~ "SJR",
      "HJRR" ~ "HJR",
      .default = bill_type,
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type} {bill_number}") |> as.character()
  ) |>
  select(UUID, session = year, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/south_dakota"))) |>
  future_pmap(scrape_text, .progress = T)

