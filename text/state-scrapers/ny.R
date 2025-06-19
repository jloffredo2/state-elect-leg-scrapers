##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape NY Bill Text
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
  
  # Step 1: Convert amendment markup FIRST before removing other tags
  # Handle underlined text (both <b><u> and standalone <u>)
  html_content <- str_replace_all(html_content, '<b><u>(.*?)</u></b>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<u>(.*?)</u>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle strikethrough text 
  html_content <- str_replace_all(html_content, '<b><s>(.*?)</s></b>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, '<s>(.*?)</s>', '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Handle bracketed deleted text like [text]
  html_content <- str_replace_all(html_content, "\\[([^\\]]+?)\\]", '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Step 2: Remove legislative boilerplate and identifiers
  # Remove line numbers (1-3 digits at start of line or surrounded by whitespace)
  html_content <- str_replace_all(html_content, "^\\s*\\d{1,3}\\s+", "")
  html_content <- str_replace_all(html_content, "\\s+\\d{1,3}\\s+", " ")
  
  # Remove legislative document codes like "LBD01214-01-3"
  html_content <- str_replace_all(html_content, "\\b[A-Z]{2,}\\d{2,}-\\d{2,}-\\d+\\b", "")
  
  # Remove explanation boilerplate - more comprehensive
  html_content <- str_replace_all(html_content, "EXPLANATION--Matter in.*?omitted\\.", "")
  html_content <- str_replace_all(html_content, "EXPLANATION--Matter in.*?\\.", "")
  html_content <- str_replace_all(html_content, "\\(underscored\\) is new; matter in brackets", "")
  html_content <- str_replace_all(html_content, "is old law to be omitted", "")
  html_content <- str_replace_all(html_content, "\\(underscored\\)", "")
  
  # Remove page/section references like "S. 310" followed by numbers
  html_content <- str_replace_all(html_content, "S\\.\\s*\\d+\\s*\\d*", "")
  
  # Step 3: Remove all HTML tags except our amendment tags
  # Remove style tags and CSS - more aggressive patterns for R
  html_content <- str_replace_all(html_content, regex("<style[^>]*>.*?</style>", dotall = TRUE), "")
  html_content <- str_replace_all(html_content, regex("<!--.*?-->", dotall = TRUE), "")
  html_content <- str_replace_all(html_content, "<style[^>]*>", "")
  html_content <- str_replace_all(html_content, "</style>", "")
  
  # Remove all other HTML tags
  tags_to_remove <- c("pre", "font", "basefont", "b", "strong", "i", "em", 
                      "p", "div", "span", "table", "tr", "td", "th", "thead", "tbody",
                      "a", "center", "meta", "code", "colgroup", "body", "html")
  
  for(tag in tags_to_remove) {
    html_content <- str_replace_all(html_content, paste0("<", tag, "[^>]*>"), "")
    html_content <- str_replace_all(html_content, paste0("</", tag, ">"), "")
  }
  
  # Remove any remaining HTML attributes and orphaned tags
  html_content <- str_replace_all(html_content, '<[^>]*class="brk"[^>]*>', "")
  html_content <- str_replace_all(html_content, 'width="\\d+"', "")
  html_content <- str_replace_all(html_content, 'size="\\d+"', "")
  
  # Step 4: Clean up consecutive amendment tags and nested strikes
  # Fix nested strike tags first
  html_content <- str_replace_all(html_content, '<strike class="amendmentDeletedText"><strike class="amendmentDeletedText">', '<strike class="amendmentDeletedText">')
  html_content <- str_replace_all(html_content, '</strike></strike>', '</strike>')
  
  # Merge consecutive amendment tags (repeat multiple times to catch all)
  for(i in 1:5) {
    html_content <- str_replace_all(html_content, '</u>\\s*<u class="amendmentInsertedText">', ' ')
    html_content <- str_replace_all(html_content, '</strike>\\s*<strike class="amendmentDeletedText">', ' ')
  }
  
  # Step 5: Final cleanup
  # Remove extra whitespace and normalize
  html_content <- str_replace_all(html_content, "\\s+", " ")
  html_content <- str_replace_all(html_content, "^\\s+|\\s+$", "")
  
  # Clean up spacing around tags
  html_content <- str_replace_all(html_content, "\\s+<", " <")
  html_content <- str_replace_all(html_content, ">\\s+", "> ")
  
  # Remove any remaining artifacts
  html_content <- str_replace_all(html_content, "_+", "")  # Remove underscores
  html_content <- str_trim(html_content)
  html_content <- str_remove_all(html_content,'EXPLANATION--Matter in <u class=\"amendmentInsertedText\">italics</u> <strike class=\"amendmentDeletedText\"> </strike> .')
  
  return(html_content)
}


build_url <- function(session, bill_number){
  glue("https://nyassembly.gov/leg/?bn={bill_number}&term={session}&Summary=Y&Text=Y")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_york'
  
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
  
    summary <- page |> html_nodes("td") |> html_text() |> tail(1)
    
    text <- page |> html_node("pre") |> as.character() |> clean_html() |> str_trim() |> str_squish()

    output <- glue("{text}\n\n Summary: {summary}")
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(output, file_name)
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'NY' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "NY"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = str_pad(bill_number, width = 5, side = "left", pad = "0"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  select(UUID, session = year, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/new_york"))) |>
  future_pmap(scrape_text)

