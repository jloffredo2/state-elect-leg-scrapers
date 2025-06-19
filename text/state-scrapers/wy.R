##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape WY Bill Text
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
library(xml2)

plan(multisession, workers = 11)

clean_html <- function(html_content) {
  
  # Step 1: Convert amendment markup FIRST - be very specific about the patterns
  # Handle blue strikethrough deletions (order matters - try both orders of attributes)
  html_content <- str_replace_all(html_content, '<span style="[^"]*text-decoration:line-through[^"]*color:#0000ff[^"]*">([^<]+)</span>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, '<span style="[^"]*color:#0000ff[^"]*text-decoration:line-through[^"]*">([^<]+)</span>', '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Handle red underline additions  
  html_content <- str_replace_all(html_content, '<span style="[^"]*text-decoration:underline[^"]*color:#ff0000[^"]*">([^<]+)</span>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<span style="[^"]*color:#ff0000[^"]*text-decoration:underline[^"]*">([^<]+)</span>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle legacy formats for backwards compatibility
  html_content <- str_replace_all(html_content, '<b><u>(.*?)</u></b>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<b><s>(.*?)</s></b>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, "\\[([^\\]]+?)\\]", '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Step 2: Remove HTML structure but keep content
  # Remove only the HTML tags, not the content inside them
  html_content <- str_replace_all(html_content, "<html[^>]*>", "")
  html_content <- str_replace_all(html_content, "</html>", "")
  html_content <- str_replace_all(html_content, "<head[^>]*>.*?</head>", "")
  html_content <- str_replace_all(html_content, "<body[^>]*>", "")
  html_content <- str_replace_all(html_content, "</body>", "")
  html_content <- str_replace_all(html_content, "<div[^>]*>", "")
  html_content <- str_replace_all(html_content, "</div>", "")
  html_content <- str_replace_all(html_content, "<p[^>]*>", "")
  html_content <- str_replace_all(html_content, "</p>", " ")
  
  # Remove spans that aren't our amendment markup
  html_content <- str_replace_all(html_content, '<span style="font-family:[^"]*"[^>]*>', "")
  html_content <- str_replace_all(html_content, '<span[^>]*font-family[^>]*>', "")
  html_content <- str_replace_all(html_content, '<span[^>]*width:[^>]*>', "")
  html_content <- str_replace_all(html_content, '<span[^>]*display:[^>]*>', "")
  html_content <- str_replace_all(html_content, '<span[^>]*-aw-[^>]*>', "")
  html_content <- str_replace_all(html_content, "</span>", "")
  
  # Remove other formatting tags
  html_content <- str_replace_all(html_content, "<b[^>]*>", "")
  html_content <- str_replace_all(html_content, "</b>", "")
  html_content <- str_replace_all(html_content, "<i[^>]*>", "")
  html_content <- str_replace_all(html_content, "</i>", "")
  html_content <- str_replace_all(html_content, "<strong[^>]*>", "")
  html_content <- str_replace_all(html_content, "</strong>", "")
  
  # Remove tables
  html_content <- str_replace_all(html_content, "<table[^>]*>", "")
  html_content <- str_replace_all(html_content, "</table>", "")
  html_content <- str_replace_all(html_content, "<tr[^>]*>", "")
  html_content <- str_replace_all(html_content, "</tr>", "")
  html_content <- str_replace_all(html_content, "<td[^>]*>", "")
  html_content <- str_replace_all(html_content, "</td>", "")
  
  # Remove links and other elements
  html_content <- str_replace_all(html_content, "<a[^>]*>", "")
  html_content <- str_replace_all(html_content, "</a>", "")
  html_content <- str_replace_all(html_content, "<meta[^>]*>", "")
  
  # Step 3: Clean up HTML entities
  html_content <- str_replace_all(html_content, "&#xa0;", " ")  # Non-breaking space
  html_content <- str_replace_all(html_content, "&#x2011;", "-")  # Non-breaking hyphen
  html_content <- str_replace_all(html_content, "&nbsp;", " ")
  html_content <- str_replace_all(html_content, "&amp;", "&")
  html_content <- str_replace_all(html_content, "&lt;", "<")
  html_content <- str_replace_all(html_content, "&gt;", ">")
  
  # Step 4: Merge consecutive amendment tags
  for(i in 1:3) {
    html_content <- str_replace_all(html_content, '</u>\\s*<u class="amendmentInsertedText">', ' ')
    html_content <- str_replace_all(html_content, '</strike>\\s*<strike class="amendmentDeletedText">', ' ')
  }
  
  # Step 6: Remove legislative boilerplate (optional - you can comment these out if you want to keep them)
  html_content <- str_replace_all(html_content, "ORIGINAL HOUSE", "")
  html_content <- str_replace_all(html_content, "BILL NO\\. \\d+", "")
  html_content <- str_replace_all(html_content, "ENROLLED ACT NO\\. \\d+, HOUSE OF REPRESENTATIVES", "")
  html_content <- str_replace_all(html_content, "FIFTY-SEVENTH LEGISLATURE OF THE STATE OF WYOMING", "")
  html_content <- str_replace_all(html_content, "\\d{4} GENERAL SESSION", "")
  html_content <- str_replace_all(html_content, "\\(END\\)", "")
  html_content <- str_replace_all(html_content, "Speaker of the House", "")
  html_content <- str_replace_all(html_content, "President of the Senate", "")
  html_content <- str_replace_all(html_content, "Governor", "")
  html_content <- str_replace_all(html_content, "TIME APPROVED: _+", "")
  html_content <- str_replace_all(html_content, "DATE APPROVED: _+", "")
  html_content <- str_replace_all(html_content, "I hereby certify that this act originated in the House\\.", "")
  html_content <- str_replace_all(html_content, "Chief Clerk", "")
  
  # Step 7: Final whitespace cleanup
  html_content <- str_replace_all(html_content, "\\s+", " ")
  html_content <- str_replace_all(html_content, "^\\s+|\\s+$", "")
  html_content <- str_trim(html_content)
  
  return(html_content)
}

build_url <- function(session, bill_number){
  glue("https://lsoservice.wyoleg.gov/api/BillInformation/{session}/{bill_number}?specialSessionValue=null")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/wyoming'
  
  url <- build_url(session, bill_number)
  
  response <- GET(url, add_headers(`Accept` = "application/json"))
  
  if(response$status_code != 200){
    message(glue("Error: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    data <- content(response, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(data, flatten = TRUE)
    
    text <- parsed$currentBillHTML |> clean_html() |> str_trim() |> str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'WY' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "WY"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SF",
      .default = bill_type,
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = str_pad(bill_number, width = 4, side = "left", pad = "0"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  select(UUID, session = year, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/wyoming"))) |>
  future_pmap(scrape_text)

