##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape VA Bill Text
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
library(polite)

plan(multisession, workers = 7)

clean_html <- function(html_content) {
  
  # Step 1: Convert Virginia legislative amendment markup
  # Handle strikethrough deletions (<s> tags) - multiple passes to catch all variations
  html_content <- str_replace_all(html_content, '<s>(.*?)</s>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, '<s>([^<]*)</s>', '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Handle italic additions (<i> tags) - multiple passes to catch all variations
  html_content <- str_replace_all(html_content, '<i>(.*?)</i>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<i>([^<]*)</i>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle any remaining plain tags that might not have been caught
  html_content <- str_replace_all(html_content, '<s>', '<strike class="amendmentDeletedText">')
  html_content <- str_replace_all(html_content, '</s>', '</strike>')
  html_content <- str_replace_all(html_content, '<i>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '</i>', '</u>')
  
  # Handle legacy formats for backwards compatibility
  html_content <- str_replace_all(html_content, '<strike>(.*?)</strike>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, '<u>(.*?)</u>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Step 2: Remove Virginia-specific navigation and metadata
  # Remove navigation elements
  html_content <- str_replace_all(html_content, '<ul id="rtNav">.*?</ul>', '')
  html_content <- str_replace_all(html_content, '<a[^>]*>.*?</a>', '')
  html_content <- str_replace_all(html_content, '<span[^>]*onclick[^>]*>.*?</span>', '')
  
  # Remove pipe characters from navigation
  html_content <- str_replace_all(html_content, '^\\s*\\|+\\s*', '')
  html_content <- str_replace_all(html_content, '\\s*\\|+\\s*', ' ')
  
  # Remove session headers
  html_content <- str_replace_all(html_content, '\\d{4} SESSION', '')
  
  # Remove document numbers
  html_content <- str_replace_all(html_content, '^\\d{9}', '')
  
  # Remove separators
  html_content <- str_replace_all(html_content, '----------', '')
  
  # Step 3: Remove HTML structure but keep content
  # Remove div containers
  html_content <- str_replace_all(html_content, '<div[^>]*>', '')
  html_content <- str_replace_all(html_content, '</div>', '')
  
  # Remove paragraph tags but keep content
  html_content <- str_replace_all(html_content, '<p[^>]*>', '')
  html_content <- str_replace_all(html_content, '</p>', ' ')
  
  # Remove center tags
  html_content <- str_replace_all(html_content, '<center[^>]*>', '')
  html_content <- str_replace_all(html_content, '</center>', ' ')
  
  # Remove header tags
  html_content <- str_replace_all(html_content, '<h\\d[^>]*>', '')
  html_content <- str_replace_all(html_content, '</h\\d>', ' ')
  
  # Remove list tags
  html_content <- str_replace_all(html_content, '<ul[^>]*>', '')
  html_content <- str_replace_all(html_content, '</ul>', '')
  html_content <- str_replace_all(html_content, '<li[^>]*>', '')
  html_content <- str_replace_all(html_content, '</li>', ' ')
  
  # Remove other formatting tags
  html_content <- str_replace_all(html_content, '<b[^>]*>', '')
  html_content <- str_replace_all(html_content, '</b>', '')
  html_content <- str_replace_all(html_content, '<strong[^>]*>', '')
  html_content <- str_replace_all(html_content, '</strong>', '')
  html_content <- str_replace_all(html_content, '<em[^>]*>', '')
  html_content <- str_replace_all(html_content, '</em>', '')
  
  # Remove span tags (except those converted to amendment markup)
  html_content <- str_replace_all(html_content, '<span[^>]*>', '')
  html_content <- str_replace_all(html_content, '</span>', '')
  
  # Remove breaks
  html_content <- str_replace_all(html_content, '<br[^>]*>', ' ')
  
  # Step 4: Clean up special characters and entities
  html_content <- str_replace_all(html_content, '&#xa0;', ' ')  # Non-breaking space
  html_content <- str_replace_all(html_content, '&nbsp;', ' ')
  html_content <- str_replace_all(html_content, '&amp;', '&')
  html_content <- str_replace_all(html_content, '&lt;', '<')
  html_content <- str_replace_all(html_content, '&gt;', '>')
  html_content <- str_replace_all(html_content, '\\r\\n', ' ')
  html_content <- str_replace_all(html_content, '\\n', ' ')
  html_content <- str_replace_all(html_content, '\\r', ' ')
  
  # Step 5: Final pass to catch any remaining unconverted tags
  # Sometimes tags don't get converted if they're malformed or split across processing
  html_content <- str_replace_all(html_content, '<s>', '<strike class="amendmentDeletedText">')
  html_content <- str_replace_all(html_content, '</s>', '</strike>')
  html_content <- str_replace_all(html_content, '<i>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '</i>', '</u>')
  
  # Merge consecutive amendment tags
  for(i in 1:3) {
    html_content <- str_replace_all(html_content, '</u>\\s*<u class="amendmentInsertedText">', ' ')
    html_content <- str_replace_all(html_content, '</strike>\\s*<strike class="amendmentDeletedText">', ' ')
  }
  
  # Step 6: Final cleanup
  # Remove extra whitespace and normalize
  html_content <- str_replace_all(html_content, '\\s+', ' ')
  html_content <- str_replace_all(html_content, '^\\s+|\\s+$', '')
  
  # Clean up spacing around tags
  html_content <- str_replace_all(html_content, '\\s+<', ' <')
  html_content <- str_replace_all(html_content, '>\\s+', '> ')
  
  html_content <- str_trim(html_content)
  
  return(html_content)
}


build_url <- function(session, bill_number){
  glue("https://legacylis.virginia.gov/cgi-bin/legp604.exe?{session}+sum+{bill_number}")
}

scrape_text <- function(UUID, session, source_houseorig, source_billtype, source_btype, bill_number) {
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/virginia'
  
  url <- build_url(session, bill_number)
  
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  
  if (response$status_code != 200) {
    message(url)
    message(glue("Failed to fetch {UUID} - status code: {response$status_code}"))
    return(NULL)
  } else {
    page <- read_html(response)
    
    # Check if server error
    if (str_detect(page |> as.character(), "Sorry, your query could not be completed")) {
      message("server error, need to retry after delay")
      
      valid_call <- FALSE
      while (!valid_call) {
        Sys.sleep(20)
        response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
        page <- read_html(response)
        valid_call <- response$status_code == 200 &&
          !str_detect(page |> as.character(), "Sorry, your query could not be completed")
      }
    }
    
    summary <- page |> html_nodes("p") |> html_text() |> tail(1) |> str_trim() |> str_squish()
    
    text_link <- page |>html_nodes("a") |>  html_attr("href") |>  str_subset("\\+ful") |>  str_subset("pdf", negate = TRUE) |>  tail(1)
    
    text_link <- glue("https://legacylis.virginia.gov/{text_link}") |> as.character()
    
    text <- bow(text_link, force = TRUE) |> scrape() |> html_node("#mainC") |>  as.character() |> clean_html() |> str_trim() |> str_squish()
    
    if (is.na(text)) {
      message("Issue getting text")
      valid_text <- FALSE
      
      while (!valid_text) {
        Sys.sleep(10)
        text <- bow(text_link, force = TRUE) |> scrape() |> html_node("#mainC") |>  as.character() |> clean_html() |> str_trim() |> str_squish()
        valid_text <- !is.na(text)
      }
    }
    
    output <- glue("Summary: {summary}\n\n{text}")
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    write_lines(output, file_name)
  }
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'VA' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "VA"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_match(
      year,
      '2001' ~ '011',
      '2002' ~ '021',
      '2003' ~ '031',
      '2004' ~ '041',
      '2005' ~ '051',
      '2006' ~ '061',
      '2007' ~ '071',
      '2008' ~ '081',
      '2009' ~ '091',
      '2010' ~ '101',
      '2011' ~ '111',
      '2012' ~ '121',
      '2013' ~ '131',
      '2014' ~ '141'
    ), 
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      "HJR" ~ "HJ",
      "SJR" ~ "SJ",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type}{bill_number}") |> str_to_upper()
  ) |>
  select(UUID, session, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/virginia"))) |>
  future_pmap(scrape_text, .progress = TRUE, .options = furrr_options(seed = TRUE))
  #pmap(scrape_text)

