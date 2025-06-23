##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape WV Bill Text
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
  
  # Step 1: Convert West Virginia legislative amendment markup (both old and new formats)
  # Handle strikethrough deletions (text-decoration: line-through) - newer format
  html_content <- str_replace_all(html_content, '<span style="[^"]*text-decoration:\\s*line-through[^"]*">([^<]*)</span>', '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Handle underline additions (text-decoration: underline) - newer format - multiple patterns
  html_content <- str_replace_all(html_content, '<span style="[^"]*text-decoration:\\s*underline[^"]*">([^<]*)</span>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<span style="text-decoration:\\s*underline">([^<]*)</span>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle nested spans with underline (more complex patterns)
  html_content <- str_replace_all(html_content, '<span[^>]*><span style="[^"]*text-decoration:\\s*underline[^"]*">([^<]*)</span></span>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle older format direct tags - more comprehensive patterns
  html_content <- str_replace_all(html_content, '<strike>(.*?)</strike>', '<strike class="amendmentDeletedText">\\1</strike>')
  html_content <- str_replace_all(html_content, '<u>(.*?)</u>', '<u class="amendmentInsertedText">\\1</u>')
  
  # Handle any remaining plain <u> tags that might not have been caught - be more aggressive
  html_content <- str_replace_all(html_content, '<u([^>]*)>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '<strike([^>]*)>', '<strike class="amendmentDeletedText">')
  
  # Also handle any remaining unclosed or malformed tags after HTML processing
  # This needs to happen after other HTML cleanup, so we'll do it again later
  
  # Handle legacy formats for backwards compatibility (but be more specific about bracketed text)
  html_content <- str_replace_all(html_content, '<b><u>(.*?)</u></b>', '<u class="amendmentInsertedText">\\1</u>')
  html_content <- str_replace_all(html_content, '<b><s>(.*?)</s></b>', '<strike class="amendmentDeletedText">\\1</strike>')
  # Only treat bracketed text as deletions if it contains actual words, not just spaces or formatting
  html_content <- str_replace_all(html_content, "\\[([a-zA-Z][^\\]]{1,50})\\]", '<strike class="amendmentDeletedText">\\1</strike>')
  
  # Step 2: Remove West Virginia-specific artifacts
  # Remove status headers
  html_content <- str_replace_all(html_content, 'ENGROSSED', '')
  
  # Remove spacing images and navigation elements
  html_content <- str_replace_all(html_content, '<img[^>]*>', '')
  html_content <- str_replace_all(html_content, '<a[^>]*>[^<]*</a>', '')
  html_content <- str_replace_all(html_content, '<a[^>]*>', '')
  html_content <- str_replace_all(html_content, '</a>', '')
  
  # Remove underscores used as dividers
  html_content <- str_replace_all(html_content, '_{5,}', '')
  
  # Remove explanatory notes at the end
  html_content <- str_replace_all(html_content, 'NOTE: The purpose of this bill[^.]+\\.', '')
  html_content <- str_replace_all(html_content, 'Strike-throughs indicate language[^.]+\\.', '')
  html_content <- str_replace_all(html_content, 'underscoring indicates new language[^.]+\\.', '')
  html_content <- str_replace_all(html_content, 'This section is new; therefore, it has been completely underscored\\.', '')
  
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
  
  # Remove blockquote tags
  html_content <- str_replace_all(html_content, '<blockquote[^>]*>', '')
  html_content <- str_replace_all(html_content, '</blockquote>', ' ')
  
  # Remove font tags
  html_content <- str_replace_all(html_content, '<font[^>]*>', '')
  html_content <- str_replace_all(html_content, '</font>', '')
  
  # Remove span tags that aren't our amendment markup
  html_content <- str_replace_all(html_content, '<span style="font-family:[^"]*"[^>]*>', '')
  html_content <- str_replace_all(html_content, '<span style="[^"]*font-family[^"]*"[^>]*>', '')
  html_content <- str_replace_all(html_content, '<span[^>]*>', '')
  html_content <- str_replace_all(html_content, '</span>', '')
  
  # Remove other formatting tags
  html_content <- str_replace_all(html_content, '<b[^>]*>', '')
  html_content <- str_replace_all(html_content, '</b>', '')
  html_content <- str_replace_all(html_content, '<i[^>]*>', '')
  html_content <- str_replace_all(html_content, '</i>', '')
  html_content <- str_replace_all(html_content, '<strong[^>]*>', '')
  html_content <- str_replace_all(html_content, '</strong>', '')
  html_content <- str_replace_all(html_content, '<em[^>]*>', '')
  html_content <- str_replace_all(html_content, '</em>', '')
  
  # Remove breaks and name anchors
  html_content <- str_replace_all(html_content, '<br[^>]*>', ' ')
  html_content <- str_replace_all(html_content, '<a name="[^"]*"></a>', '')
  
  # Step 4: Clean up special characters and entities
  html_content <- str_replace_all(html_content, '&#xa0;', ' ')  # Non-breaking space
  html_content <- str_replace_all(html_content, '&nbsp;', ' ')
  html_content <- str_replace_all(html_content, '&amp;', '&')
  html_content <- str_replace_all(html_content, '&lt;', '<')
  html_content <- str_replace_all(html_content, '&gt;', '>')
  html_content <- str_replace_all(html_content, '\\r\\n', ' ')
  html_content <- str_replace_all(html_content, '\\n', ' ')
  html_content <- str_replace_all(html_content, '\\r', ' ')
  
  # Step 5: Final fix for any remaining plain tags after all HTML cleanup
  # Sometimes tags get malformed during the HTML cleanup process
  # Be more careful to only match properly formed tags
  html_content <- str_replace_all(html_content, '<u>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '<strike>', '<strike class="amendmentDeletedText">')
  
  # Fix any doubled closing brackets that might have been created
  html_content <- str_replace_all(html_content, '<u class="amendmentInsertedText">>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '<strike class="amendmentDeletedText">>', '<strike class="amendmentDeletedText">')
  
  # Clean up any other malformed patterns
  html_content <- str_replace_all(html_content, '>>+', '>')
  
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


build_url <- function(session, source_houseorig, source_billtype, source_btype, bill_number){
  if(source_btype == 'bill'){
    glue("https://www.wvlegislature.gov/Bill_Status/Bills_history.cfm?input={bill_number}&year={session}&sessiontype=RS&btype={source_btype}")
  } else{
    glue("https://www.wvlegislature.gov/Bill_Status/Resolution_History.cfm?year={session}&sessiontype=RS&input4={bill_number}&billtype={source_billtype}&houseorig={source_houseorig}&btype={source_btype}")
  }
}

scrape_text <- function(UUID, session, source_houseorig, source_billtype, source_btype, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/west_virginia'
  
  url <- build_url(session, source_houseorig, source_billtype, source_btype, bill_number)
  
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = FALSE, followlocation = TRUE))
  
  if(response$status_code != 200){
    message(url)
    message(glue("Failed to fetch {UUID} - status code: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    page_text <- httr::content(response, "text")
    page <- read_html(page_text)
    
    text_link <- page |> html_nodes("a") |> html_attr("href") |> str_subset("bills_text.cfm") |> tail(1)
    text_link <- glue("https://www.wvlegislature.gov/Bill_Status/{text_link}")
    
    text <- read_html(text_link) |> html_node(".textcontainer") |> as.character() |> clean_html() |> str_trim() |> str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'WV' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "WV"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    source_btype = case_match(
      bill_type,
      "H" ~ "bill",
      "S" ~ "bill",
      'HJR' ~ "res",
      'SJR' ~ "res",
      'HCR' ~ "res",
      'SCR' ~ "res",
      .default = "bill"
    ),
    source_billtype = case_match(
      bill_type,
      "HJR" ~ "jr",
      "SJR" ~ "jr",
      'HCR' ~ "cr",
      'SCR' ~ "cr",
      .default = NA_character_
    ),
    source_houseorig = ifelse(str_detect(bill_type, "^H"), "h", "s"),
    bill_number = str_extract(bill_id, "[0-9]+$"),
  ) |>
  select(UUID, session = year, source_houseorig, source_billtype, source_btype, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/west_virginia"))) |>
  future_pmap(scrape_text)

