library(tidyverse)
library(rvest)
library(httr2)
library(furrr)
library(future)
library(glue)
library(fs)

rm(list = ls())
gc()

TEXT_DIR <- "/Users/josephloffredo/Dropbox (MIT)/election_bill_text"

plan(multisession, workers = 11, gc = TRUE)

# Load bill data ----------------------------------------------------------
ncsl_bill_database_archived <- read.csv("https://github.com/jloffredo2/state-elect-law-db/raw/refs/heads/main/output/ncsl_bill_database_2011_2024.csv") |> 
  mutate(SOURCE = 'NCSL') |>
  separate(BILLNUM, into = c("chamber","bill_number"),sep = "(?<=\\D)(?=\\d)",remove = F) |>
  mutate(
    #bill_number = str_pad(as.character(bill_number),width = 4,side = "left",pad = "0"),
    PREFILEDATE = as_date(PREFILEDATE), 
    INTRODUCEDDATE = as_date(INTRODUCEDDATE), 
    YEAR2 = ifelse(!is.na(PREFILEDATE) & year(PREFILEDATE) < year(INTRODUCEDDATE), year(PREFILEDATE), NA),
    LOOKUP = ifelse(is.na(YEAR2), UUID, str_c(STATE, YEAR2,BILLNUM)),
    BILLNUM = str_c(chamber, bill_number, sep = ""),
  ) |>
  select(-c(chamber, bill_number))

## Remove duplicates
ncsl_bill_database_archived <- ncsl_bill_database_archived |>
  distinct(YEAR, BILLNUM, AUTHORNAME, AUTHORPARTY, PREFILEDATE, INTRODUCEDDATE, LASTACTIONDATE, .keep_all = TRUE) |>
  arrange(UUID)

## Patch UUID for prefiled bills
prefiled_bills <- ncsl_bill_database_archived |> filter(!is.na(YEAR2)) |> mutate(YEAR = YEAR2) |>
  mutate(UUID = str_c(STATE, YEAR, BILLNUM))

ncsl_bill_database_archived <- rbind(ncsl_bill_database_archived, prefiled_bills) |> select(-c(YEAR2))

#### Current db ####
ncsl_bill_database <- read.csv("https://raw.githubusercontent.com/jloffredo2/state-elect-law-db/main/output/ncsl_bill_database.csv") |> 
  mutate(SOURCE = 'NCSL') |>
  separate(BILLNUM, into = c("chamber","bill_number"),sep = "(?<=\\D)(?=\\d)",remove = F) |>
  mutate(
    #bill_number = str_pad(as.character(bill_number),width = 4,side = "left",pad = "0"),
    PREFILEDATE = as_date(PREFILEDATE), 
    INTRODUCEDDATE = as_date(INTRODUCEDDATE), 
    YEAR2 = ifelse(!is.na(PREFILEDATE) & year(PREFILEDATE) < year(INTRODUCEDDATE), year(PREFILEDATE), NA),
    LOOKUP = ifelse(is.na(YEAR2), UUID, str_c(STATE, YEAR2,BILLNUM)),
    BILLNUM = str_c(chamber, bill_number, sep = ""),
  ) |>
  select(-c(chamber, bill_number))

## Remove duplicates
ncsl_bill_database <- ncsl_bill_database |>
  distinct(YEAR, BILLNUM, AUTHORNAME, AUTHORPARTY, PREFILEDATE, INTRODUCEDDATE, LASTACTIONDATE, .keep_all = TRUE) |>
  arrange(UUID)

## Patch UUID for prefiled bills
prefiled_bills <- ncsl_bill_database |> filter(!is.na(YEAR2)) |> mutate(YEAR = YEAR2) |>
  mutate(UUID = str_c(STATE, YEAR, BILLNUM))

ncsl_bill_database <- rbind(ncsl_bill_database, prefiled_bills) |> select(-c(YEAR2))

#### VRL ####
vrl_bill_database <- read.csv("https://raw.githubusercontent.com/jloffredo2/state-elect-law-db/main/output/vrl_bill_database.csv") |>
  mutate(SOURCE = 'VRL') |>
  separate(BILLNUM, into = c("chamber","bill_number"),sep = "(?<=\\D)(?=\\d)",remove = F) |>
  mutate(
    #bill_number = str_pad(as.character(bill_number),width = 4,side = "left",pad = "0"),
    PREFILEDATE = as_date(PREFILEDATE), 
    INTRODUCEDDATE = as_date(INTRODUCEDDATE), 
    YEAR2 = ifelse(!is.na(PREFILEDATE) & year(PREFILEDATE) < year(INTRODUCEDDATE), year(PREFILEDATE), NA),
    LOOKUP = ifelse(is.na(YEAR2), UUID, str_c(STATE, YEAR2, BILLNUM)),
    BILLNUM = str_c(chamber, bill_number, sep = "")
  ) |>
  select(-c(chamber, bill_number))

vrl_bill_database <- vrl_bill_database |>
  arrange(desc(UUID)) |>
  distinct(YEAR, BILLNUM, AUTHORNAME, AUTHORPARTY, PREFILEDATE, INTRODUCEDDATE, LASTACTIONDATE, .keep_all = TRUE) |>
  arrange(UUID)

prefiled_bills <- vrl_bill_database |> filter(!is.na(YEAR2)) |> mutate(YEAR = YEAR2)
vrl_bill_database <- rbind(vrl_bill_database, prefiled_bills) |> select(-c(YEAR2)) |>
  mutate(UUID = str_c(STATE, YEAR, BILLNUM))

bills <- rbind(
  ncsl_bill_database_archived |> select(UUID, LOOKUP, BILLNUM, YEAR, STATE, BILLTEXTURL),
  ncsl_bill_database |> select(UUID, LOOKUP, BILLNUM, YEAR, STATE, BILLTEXTURL),
  vrl_bill_database |> select(UUID, LOOKUP, BILLNUM, YEAR, STATE, BILLTEXTURL)) |>
  mutate(UUID2 = UUID) |>
  pivot_longer(cols = c(UUID2, LOOKUP),values_to = "LOOKUP",names_to = NULL,) |>
  distinct(LOOKUP, .keep_all = T) |>
  select(UUID = LOOKUP, BILLTEXTURL) |>
  drop_na(BILLTEXTURL)

bills <- bills |>
  mutate(BILLTEXTURL = map(BILLTEXTURL, function(url_str) {
    url_str <- str_replace_all(url_str, "[\\[\\]\"]", "")
    str_extract_all(url_str, "http[s]?://[^\\s,]+")[[1]]
  })) |>
  unnest(BILLTEXTURL) |>
  distinct() |>
  # remove &mode=current_text so we can pull all versions
  mutate(BILLTEXTURL = str_remove(BILLTEXTURL, "&mode=current_text"))

# Scraper functions -------------------------------------------------------
# Function to clean up HTML content
clean_html <- function(html_content) {
  html_content <- str_replace_all(html_content, "<p[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</p>", " ")
  html_content <- str_replace_all(html_content, "<div[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</div>", " ")
  html_content <- str_replace_all(html_content, "<a[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</a>", " ")
  html_content <- str_replace_all(html_content, "<span[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</span>", " ")
  html_content <- str_replace_all(html_content, "<b[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</b>", " ")
  html_content <- str_replace_all(html_content, "<i[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</i>", " ")
  html_content <- str_replace_all(html_content, "</td>", " ")
  html_content <- str_replace_all(html_content, "</tr>", " ")
  html_content <- str_replace_all(html_content, "\\s+", " ")
  html_content <- str_trim(html_content)
  return(html_content)
}

version_pull <- function(url, original_url, uuid){
  TEXT_DIR = "/Users/josephloffredo/Dropbox (MIT)/election_bill_text"
  
  html <- read_html(url)
  key <- (html |> html_elements(".key") |> html_text())[1]
  author <- (html |> html_elements("#text-identifier table tr:nth-child(1) td:nth-child(2)") |> html_text())[1]
  version <- (html |> html_elements("tr:nth-child(2) td:nth-child(2)") |> html_text())[1]
  version_date <- (html |> html_elements("tr:nth-child(3) td:nth-child(2)") |> html_text())[1]
  
  html_format_text <- html |> html_elements(".documentBody") |> as.character() |> 
    clean_html() |>
    str_trim() |>
    str_squish()
  
  file_name <- str_c(key, version, version_date, sep = "_") |>
    str_replace_all("[^A-Za-z0-9]", "_") |>  # Removed \\/ from the pattern
    str_squish()
  
  message("Writing HTML format text file: ", file_name)
  writeLines(html_format_text, glue("{TEXT_DIR}/LexisNexis/{uuid}/{file_name}_html.txt"))
  
  Sys.sleep(runif(1, 1, 2) |> as.integer())
}

# Scraper function
scrape_text <- function(UUID, BILLTEXTURL){
  TEXT_DIR = "/Users/josephloffredo/Dropbox (MIT)/election_bill_text"
  # Ensure the directory exists
  dir_create(glue("{TEXT_DIR}/LexisNexis/{UUID}"))
  
  # Start scrape
  message(BILLTEXTURL)
  html <- read_html(BILLTEXTURL)
  
  # add check to see if empty; if so, return NULL
  if(str_detect(html |> html_text(),"There are no text versions currently associated with this ID.")) {
    message(glue("No text versions found for {UUID}"))
  } else {
    versions <- html |> html_elements("a") |> html_attr("href") |> na.omit()
    
    htmls <- str_c('http://custom.statenet.com', versions)
    scraped_versions <- lapply(htmls, function(ver_url) version_pull(ver_url, original_url = BILLTEXTURL, uuid = UUID))
  }
}

# Scrape text -------------------------------------------------------------
bills |> 
  filter(!(UUID %in% list.files(path = "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/LexisNexis"))) |>
  future_pmap(scrape_text, .progress = T)
