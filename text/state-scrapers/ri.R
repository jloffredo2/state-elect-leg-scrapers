##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape RI Bill Text
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
  
  # Step 1: Remove <style> blocks, comments, and head section first
  html_content <- str_replace_all(html_content, "(?is)<style[^>]*>.*?</style>", " ")
  html_content <- str_replace_all(html_content, "(?is)<head[^>]*>.*?</head>", " ")
  html_content <- str_replace_all(html_content, "(?is)<!--.*?-->", " ")
  html_content <- str_replace_all(html_content, "<!DOCTYPE[^>]*>", " ")
  html_content <- str_replace_all(html_content, "<\\?xml[^>]*\\?>", " ")
  
  # Step 2: First remove all HTML tags except underline and strikethrough formatting
  # Temporarily mark underlined text (amendments) with placeholders
  html_content <- str_replace_all(html_content, '<span style="text-decoration:underline;">(.*?)</span>', '§§UNDERLINE_START§§\\1§§UNDERLINE_END§§')
  html_content <- str_replace_all(html_content, '<u>(.*?)</u>', '§§UNDERLINE_START§§\\1§§UNDERLINE_END§§')
  
  # Mark strikethrough text (deletions) with placeholders  
  html_content <- str_replace_all(html_content, '<span style="text-decoration:line-through;">(.*?)</span>', '§§STRIKE_START§§\\1§§STRIKE_END§§')
  html_content <- str_replace_all(html_content, '<s>(.*?)</s>', '§§STRIKE_START§§\\1§§STRIKE_END§§')
  html_content <- str_replace_all(html_content, '<del>(.*?)</del>', '§§STRIKE_START§§\\1§§STRIKE_END§§')
  html_content <- str_replace_all(html_content, '<strike>(.*?)</strike>', '§§STRIKE_START§§\\1§§STRIKE_END§§')
  
  # Step 3: Remove ALL remaining HTML tags
  html_content <- str_replace_all(html_content, '<[^>]*>', ' ')
  
  # Step 4: Convert placeholders back to proper amendment tags
  html_content <- str_replace_all(html_content, '§§UNDERLINE_START§§', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '§§UNDERLINE_END§§', '</u>')
  html_content <- str_replace_all(html_content, '§§STRIKE_START§§', '<strike class="amendmentDeletedText">')
  html_content <- str_replace_all(html_content, '§§STRIKE_END§§', '</strike>')
  
  # Step 4: Clean up HTML entities
  html_content <- str_replace_all(html_content, "&amp;nbsp|&nbsp;|&amp;nbsp;", " ")
  html_content <- str_replace_all(html_content, "&amp;", "&")
  html_content <- str_replace_all(html_content, "&lt;", "<")
  html_content <- str_replace_all(html_content, "&gt;", ">")
  html_content <- str_replace_all(html_content, "&quot;", '"')
  html_content <- str_replace_all(html_content, "&#39;", "'")
  
  # Step 5: Remove page indicators and other artifacts
  html_content <- str_replace_all(html_content, "-\\s*Page\\s*\\d*\\s*of\\s*\\d*", "")
  html_content <- str_replace_all(html_content, "Page\\s*\\d*\\s*of\\s*\\d*", "")
  html_content <- str_replace_all(html_content, "LC\\d{5,6}(?:\\s*-\\s*Page\\s*\\d+\\s*of\\s*\\d+)?", "")
  
  # Step 6: Remove line numbers (patterns like "1", "2", "1-1", "1-2", etc. at start of lines)
  html_content <- str_replace_all(html_content, "\\b\\d{1,2}(-\\d{1,2})?\\b(?=\\s)", " ")
  
  # Step 7: Remove common boilerplate patterns
  html_content <- str_replace_all(html_content, "LC\\d{5,6}(?:\\s*-\\s*Page\\s*\\d+\\s*of\\s*\\d+)?", "")
  html_content <- str_replace_all(html_content, "={3,}", "")
  html_content <- str_replace_all(html_content, "\\*{3,}", "")
  html_content <- str_replace_all(html_content, "_{3,}", "")
  
  # Step 7: Remove bill headers and metadata
  html_content <- str_replace_all(html_content, "\\d{4}\\s*--\\s*S\\s*\\d+", "")
  html_content <- str_replace_all(html_content, "STATE\\s+OF\\s+RHODE\\s+ISLAND", "")
  html_content <- str_replace_all(html_content, "IN\\s+GENERAL\\s+ASSEMBLY", "")
  html_content <- str_replace_all(html_content, "JANUARY\\s+SESSION,\\s*A\\.D\\.\\s*\\d{4}", "")
  
  # Step 8: Remove introduction metadata
  html_content <- str_replace_all(html_content, "Introduced\\s+By:", "")
  html_content <- str_replace_all(html_content, "Date\\s+Introduced:", "")
  html_content <- str_replace_all(html_content, "Referred\\s+To:", "")
  html_content <- str_replace_all(html_content, "Senators?\\s+[A-Za-z,\\s]+", "")
  html_content <- str_replace_all(html_content, "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s*\\d{4}", "")
  html_content <- str_replace_all(html_content, "Senate\\s+\\w+", "")
  html_content <- str_replace_all(html_content, "Placed\\s+on\\s+Senate\\s+Calendar", "")
  
  # Step 9: Clean up explanation section
  html_content <- str_replace_all(html_content, "EXPLANATION", "SUMMARY:")
  html_content <- str_replace_all(html_content, "BY\\s+THE\\s+LEGISLATIVE\\s+COUNCIL", "")
  
  # Step 10: Remove standalone "OF" that appears in headers
  html_content <- str_replace_all(html_content, "\\bOF\\b(?=\\s+(AN\\s+ACT|SUMMARY))", "")
  
  # Step 11: Fix "A N   A C T" spacing issues
  html_content <- str_replace_all(html_content, "\\bA\\s+N\\s+A\\s+C\\s+T\\b", "AN ACT")
  
  # Step 12: Clean up enactment clause
  html_content <- str_replace_all(html_content, "It\\s+is\\s+enacted\\s+by\\s+the\\s+General\\s+Assembly\\s+as\\s+follows:", "")
  
  # Step 13: Aggressively merge adjacent amendment tags and collapse whitespace within them
  # Repeat multiple times to catch all cases
  for (i in 1:5) {
    # Merge adjacent insertions
    html_content <- str_replace_all(html_content, '</u>\\s*<u class="amendmentInsertedText">', ' ')
    # Merge adjacent deletions  
    html_content <- str_replace_all(html_content, '</strike>\\s*<strike class="amendmentDeletedText">', ' ')
    # Clean up excessive whitespace within tags
    html_content <- str_replace_all(html_content, '(<u class="amendmentInsertedText">)\\s+', '\\1')
    html_content <- str_replace_all(html_content, '\\s+(</u>)', '\\1')
    html_content <- str_replace_all(html_content, '(<strike class="amendmentDeletedText">)\\s+', '\\1')
    html_content <- str_replace_all(html_content, '\\s+(</strike>)', '\\1')
  }
  
  # Step 14: Remove empty amendment tags
  html_content <- str_replace_all(html_content, '<u class="amendmentInsertedText">\\s*</u>', '')
  html_content <- str_replace_all(html_content, '<strike class="amendmentDeletedText">\\s*</strike>', '')
  
  # Step 15: Fix punctuation spacing
  html_content <- str_replace_all(html_content, "\\s+([.,;:!?])", "\\1")
  html_content <- str_replace_all(html_content, "([.,;:!?])\\s+", "\\1 ")
  
  # Step 16: Normalize whitespace and clean up
  html_content <- str_replace_all(html_content, "[\\r\\n]+", " ")
  html_content <- str_replace_all(html_content, "\\s{2,}", " ")
  html_content <- str_trim(html_content)
  
  # Step 17: Final cleanup - remove any remaining artifacts and normalize spacing
  html_content <- str_replace_all(html_content, "^\\s*(AN\\s+ACT|SUMMARY:)", "\\1")
  # Remove any trailing page indicators that might remain
  html_content <- str_replace_all(html_content, "\\s*-\\s*Page.*$", "")
  html_content <- str_replace_all(html_content, "\\s*Page\\s*\\d*\\s*of\\s*\\d*\\s*$", "")
  
  return(html_content)
}

build_url <- function(session, chamber, bill_number){
  glue("https://webserver.rilegislature.gov/BillText{session}/{chamber}Text{session}/{bill_number}.htm")
}

scrape_text <- function(UUID, session, chamber, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/rhode_island'
  
  url <- build_url(session, chamber, bill_number)
  
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
  filter(STATE == 'RI' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "RI"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = str_remove(year, "^20"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    chamber = ifelse(str_detect(bill_type, "H"), "House", "Senate"),
    chamber_bill_type = ifelse(chamber == "House", "H", "S"),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = str_pad(bill_number, width = 4, side = "left", pad = "0"),
    bill_number = glue("{chamber_bill_type}{bill_number}")
  ) |>
  select(UUID, session, chamber, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/rhode_island"))) |>
  future_pmap(scrape_text, .progress = T)

