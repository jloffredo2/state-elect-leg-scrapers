##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape SC Bill Text
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

plan(multisession, workers = 11)

clean_html <- function(html_content) {
  library(xml2)
  library(stringr)
  library(rvest)
  
  html_content <- as.character(html_content)
  doc <- read_html(html_content)
  
  # Remove HTML comments
  xml_find_all(doc, ".//comment()") |> xml_remove()
  
  # Tag underlines and strikethroughs with class
  xml_find_all(doc, ".//u") |> xml_set_attr("class", "amendmentInsertedText")
  xml_find_all(doc, ".//strike") |> xml_set_attr("class", "amendmentDeletedText")
  
  # Remove all other tags, flatten them
  body <- xml_find_first(doc, "//body")
  nodes <- xml_find_all(body, ".//*")
  for (node in nodes) {
    tag <- xml_name(node)
    cls <- xml_attr(node, "class")
    if (!((tag == "u" && cls == "amendmentInsertedText") ||
          (tag == "strike" && cls == "amendmentDeletedText"))) {
      xml_set_name(node, "span")
      xml_attrs(node) <- NULL
    }
  }
  
  # Convert to string
  text <- as.character(body)
  
  # Clean and strip
  text |>
    str_replace_all("(?i)<(?!/?(u|strike)(\\s|>))[^>]+>", "") |>  # keep only <u> and <strike>
    str_replace_all("&nbsp;|&#xA0;", " ") |>                      # spaces
    str_replace_all("&#167;", "§") |>                            # legal section symbol
    str_replace_all("&amp;", "&") |>                             # ampersand
    str_replace_all("(?is)(This web page was last updated.*)", "") |>  # remove trailing footer
    str_replace_all("(?i)<center>_*</center>", "") |>                 # remove page decorations
    str_replace_all("(?i)\\s+", " ") |>                          # collapse whitespace
    str_trim() |>
    str_remove("<strike class=\"amendmentDeletedText\">Indicates Matter Stricken</strike> <u class=\"amendmentInsertedText\">Indicates New Matter</u>")
}

build_url <- function(session, bill_number){
  glue("https://www.scstatehouse.gov/sess{session}/bills/{bill_number}.htm")
}

scrape_text <- function(UUID, session, bill_number){
  message(UUID)
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/south_carolina'
  
  url <- build_url(session, bill_number)
  
  response <- GET(url, config = httr::config(ssl_verifypeer = FALSE))
  
  if(response$status_code != 200){
    message(glue("Error: {response$status_code}"))
    return(NULL)
  } else{
    dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
    
    page <- read_html(response)
    
    text <- page |> 
      as.character() |> 
      str_remove(regex(".*\\(Text matches printed bills\\.  Document has been reformatted to meet World Wide Web specifications\\.\\)", dotall = TRUE)) |> 
      clean_html() |> 
      str_trim() |> 
      str_squish()
    
    file_name <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{UUID}_html.txt")
    
    write_lines(text, file_name)
  }
  
}

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'SC' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "SC"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_when(
      year %in% c('2001', '2002') ~ '114_2001-2002',
      year %in% c('2003', '2004') ~ '115_2003-2004',
      year %in% c('2005', '2006') ~ '116_2005-2006',
      year %in% c('2007', '2008') ~ '117_2007-2008',
      year %in% c('2009', '2010') ~ '118_2009-2010',
      year %in% c('2011', '2012') ~ '119_2011-2012',
      year %in% c('2013', '2014') ~ '120_2013-2014'
    ),
    bill_number = str_extract(bill_id, "[0-9]+$")
  ) |>
  select(UUID, session, bill_number)

master |>
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/south_carolina"))) |>
  future_pmap(scrape_text)

