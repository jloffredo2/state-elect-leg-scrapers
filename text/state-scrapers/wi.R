##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape WI Bill Text
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
library(googlesheets4)
library(xml2)

plan(multisession, workers = 11)

clean_html <- function(html_content) {
  doc <- read_html(html_content)
  body <- xml_find_first(doc, '//*[@id="document"]')
  
  extract_text <- function(node) {
    if (xml_type(node) == "element") {
      # Skip line number spans
      if (xml_name(node) == "span" && isTRUE(xml_attr(node, "class") == "lineNumber")) {
        return("")
      }
      
      style <- xml_attr(node, "style")
      children <- xml_contents(node)
      text_parts <- map_chr(children, extract_text)
      combined_text <- str_trim(paste(text_parts, collapse = ""))
      
      if (!is.na(style)) {
        if (str_detect(style, "text-decoration:\\s*underline")) {
          return(glue('<u class="amendmentInsertedText">{combined_text}</u>'))
        }
        if (str_detect(style, "text-decoration:\\s*line-through")) {
          return(glue('<strike class="amendmentDeletedText">{combined_text}</strike>'))
        }
      }
      
      return(combined_text)
    }
    
    if (xml_type(node) == "text") {
      return(xml_text(node))
    }
    
    return("")
  }
  
  result <- paste0(map_chr(xml_contents(body), extract_text), collapse = "")
  result <- str_replace_all(result, "\\s+", " ") |> str_trim()
  
  result <- str_remove_all(result , "Up Up")
  result <- str_remove_all(result , "Down Down")
  result <- str_remove_all(result , "\\(End\\)")
  return(result)
}


build_url <- function(session, bill_number){
  glue("https://docs.legis.wisconsin.gov/{session}/related/proposals/{bill_number}")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/wisconsin'
  
  url <- build_url(session, bill_number)
  
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  
  if(response$status_code != 200){
    message(url)
    message(glue("Failed to fetch {UUID} - status code: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    page_text <- httr::content(response, "text")
    page <- read_html(page_text)
    
    text <- page |> html_node("#document") |> as.character() |> clean_html() |> str_trim() |> str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
  
}

old_bill_list <- read_sheet("1MR4o6IQR93YYoybx6H8GjCfhSbyM4ePzf3Le0V7Ssk8") |>
  janitor::clean_names() |>
  mutate(
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "AB" ~ "A",
      "SB" ~ "S",
    ),
    bill_number_uuid = str_extract(bill_number, "[0-9]+$"),
    UUID = glue("WI{session}{bill_type}{bill_number_uuid}"),
    bill_number = str_to_lower(bill_number),
    session = as.character(session)
  ) |>
  select(UUID, session, bill_number)

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'WI' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "WI"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_when(
      year %in% c("1995", "1996") ~ "1995",
      year %in% c("1997", "1998") ~ "1997",
      year %in% c("1999", "2000") ~ "1999",
      year %in% c("2001", "2002") ~ "2001",
      year %in% c("2003", "2004") ~ "2003",
      year %in% c("2005", "2006") ~ "2005",
      year %in% c("2007", "2008") ~ "2007",
      year %in% c("2009", "2010") ~ "2009",
      year %in% c("2011", "2012") ~ "2011",
      year %in% c("2013", "2014") ~ "2013"
    ),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "A" ~ "AB",
      "S" ~ "SB",
      .default = bill_type,
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type}{bill_number}") |> str_to_lower()
  ) |>
  select(UUID, session, bill_number) |>
  bind_rows(old_bill_list)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/wisconsin"))) |>
  future_pmap(scrape_text)

