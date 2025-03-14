##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape TX Bill Text
## Date: February 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(rvest)
library(httr2)
library(furrr)
library(future)
library(glue)
library(threadr)
library(RCurl)
library(fs)
library(googlesheets4)

plan(multisession, workers = 11)

# Functions
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
  html_content <- str_replace_all(html_content, "<td[^>]*>", " ")
  html_content <- str_replace_all(html_content, "<meta[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</td>", " ")
  html_content <- str_replace_all(html_content, "</tr>", " ")
  html_content <- str_replace_all(html_content, "<tr[^>]*>", " ")
  html_content <- str_replace_all(html_content, "\\s+", " ")
  html_content <- str_replace_all(html_content, "<center[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</center[^>]*>", " ")
  html_content <- str_replace_all(html_content, "<table[^>]*>", " ")
  html_content <- str_replace_all(html_content, "</table[^>]*>", " ")
  html_content <- str_replace_all(html_content, "\\[", " ")
  html_content <- str_replace_all(html_content, "\\]", " ")
  html_content <- str_replace_all(html_content, "</u> <u>", " ")
  html_content <- str_replace_all(html_content, "</u><u>", " ")
  html_content <- str_replace_all(html_content, '<u>', '<u class="amendmentInsertedText">')
  html_content <- str_replace_all(html_content, '<s>', '<strike class="amendmentDeletedText">')
  html_content <- str_replace_all(html_content, '</s>', '</strike>')
  html_content <- str_replace_all(html_content, ' </pre>', ' ')

  html_content <- str_trim(html_content)
  return(html_content)
}
# Use state website to see all text versions
retrieve_file_names <- function(session, bill_id){
  bill_id <- str_replace_all(bill_id, " ", "")
  url <- glue("https://capitol.texas.gov/BillLookup/Text.aspx?LegSess={session}&Bill={bill_id}")
  
  # Retrieve the HTML
  html <- read_html(url)
  # Get the text links in html format
  html |> html_elements("a") |> html_attr("href") |> str_subset("billtext/html") |> str_remove_all("/tlodocs/[0-9]{2}R/billtext/html/")
}

# Build path for downloading from FTP
get_file_path <- function(session, bill_id){
  FTP_PATH <- 'ftp://ftp.legis.state.tx.us/bills'
  # Parse bill number
  bill_type <- str_extract(bill_id, "^[A-Z]+")
  bill_type <- case_match(
    bill_type,
    "HCR" ~ "HC",
    "SCR" ~ "SC",
    "HJR" ~ "HJ",
    "SJR" ~ "SJ",
    .default = bill_type
  )

  bill_number <- as.numeric(str_extract(bill_id, "[0-9]+$"))
  
  start_bill <- floor((bill_number) / 100) * 100
  end_bill <- start_bill + 99
  
  if(bill_number < 100){
    start_bill <- 1
    end_bill <- 99
  }
  start_bill <- str_pad(start_bill, 5, pad = "0", side = "left")
  end_bill <- str_pad(end_bill, 5, pad = "0", side = "left")
  
  # Dynamically create the bucket label using the correct 5-digit format
  bucket_label <- glue("{bill_type}{start_bill}_{bill_type}{end_bill}")
  
  # Bill Category
  bill_category <- case_match(
    bill_type,
    "HB" ~ "house_bills",
    "SB" ~ "senate_bills",
    "HJR" ~ "house_joint_resolutions",
    "HJ" ~ "house_joint_resolutions",
    "SJR" ~ "senate_joint_resolutions",
    "SJ" ~ "senate_joint_resolutions",
    "HR" ~ "house_resolutions",
    "SR" ~ "senate_resolutions",
    "HCR" ~ "house_concurrent_resolutions",
    "HC" ~ "house_concurrent_resolutions",
    "SCR" ~ "senate_concurrent_resolutions",
    "SC" ~ "senate_concurrent_resolutions"
  )
  
  
  dir <- glue("{FTP_PATH}/{session}/billtext/html/{bill_category}/{bucket_label}")
  
  # Get text file names
  file_names <- retrieve_file_names(session, bill_id)
  
  glue("{dir}/{file_names}")
}

# Download html files
download_bill_text <- function(UUID, file_path){
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/texas'
  
  dir_create(glue("{TEXT_OUTPUT_PATH}/{UUID}"))
  file_name <- basename(file_path) |> str_remove_all(".htm$")
  # Create destination file name
  
  raw_dest_file <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}_raw.htm")
  
  download_ftp_file_with_case <- function(file_path, raw_dest_file) {
    extensions <- c(".htm", ".HTM")
    for (ext in extensions) {
      # Modify the file path based on the extension
      temp_file_path <- str_replace(file_path, "(\\.htm|\\.HTM)$", ext)
      
      # Try downloading the file
      tryCatch({
        download_ftp_file(file_remote = temp_file_path, file_local = raw_dest_file)
        message(glue("Downloaded: {temp_file_path}"))
        return(invisible()) 
      }, error = function(e) {
        # If the file doesn't exist or another error occurs, proceed to the next extension
        message(glue("Failed to download: {temp_file_path}"))
      })
    }
    stop("Both file versions failed to download.")  # If both fail
  }
  download_ftp_file_with_case(file_path, raw_dest_file)
  
  return(raw_dest_file)
}

process_bill_text <- function(UUID, session, file_path){
  TEXT_OUTPUT_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/texas'
  
  file_name <- basename(file_path) |> str_replace_all(c("_raw.htm$" = "", "_raw.HTM$"=""))
  processed_html_dest_file <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}_html.txt")
  processed_plain_dest_file <- glue("{TEXT_OUTPUT_PATH}/{UUID}/{file_name}_plain.txt")
  
  html <- read_html(file_path)
  
  if(session %in% c("74R","75R","76R","77R")){
    html_format <- html |> html_element("pre") |> as.character() |> str_replace_all(c("\n[0-9]{1,3}-[0-9]{1,3}"=" ","\n [0-9]{1,3}-[0-9]{1,3}"=" ")) |> clean_html() |> str_trim() |> str_squish()
    raw_bill_text <- html |> html_element("pre") |> html_text() |> str_trim() |> str_squish()
  } else{
    html_format <- (html |> html_elements("table"))[2] |> as.character() |> clean_html() |> str_trim() |> str_squish()
    raw_bill_text <- (html |> html_elements("table"))[2] |> html_text() |> str_trim() |> str_squish()
  }
  
  # Debugging logs
  message("Writing plain text file: ", processed_plain_dest_file)
  writeLines(raw_bill_text, processed_plain_dest_file)
  
  message("Writing HTML format text file: ", processed_html_dest_file)
  writeLines(html_format, processed_html_dest_file)
}

scrape_text <- function(UUID, session = NA, bill_number = NA){
  year <- str_extract(UUID, "[0-9]{4}")
  
  if(is.na(session)){
    session <- case_match(
      year,
      "2014" ~ "83R",
      "2013" ~ "83R",
      "2012" ~ "82R",
      "2011" ~ "82R",
      "2010" ~ "81R",
      "2009" ~ "81R",
      "2008" ~ "80R",
      "2007" ~ "80R",
      "2006" ~ "79R",
      "2005" ~ "79R",
      "2004" ~ "78R",
      "2003" ~ "78R",
      "2002" ~ "77R",
      "2001" ~ "77R",
      "2000" ~ "76R",
      "1999" ~ "76R",
      "1998" ~ "75R",
      "1997" ~ "75R",
      "1996" ~ "74R",
      "1995" ~ "74R"
    )
  }
  
  if(is.na(bill_number)){
    bill_number <- str_remove_all(UUID, "^TX[0-9]{4}")
    bill_number <- case_when(
      str_detect(bill_number, "^S[0-9]") ~ glue("SB {str_remove(bill_number, 'S')}"),
      str_detect(bill_number, "^H[0-9]") ~ glue("HB {str_remove(bill_number, 'H')}"),
      TRUE ~ glue("{str_extract(bill_number, '^[A-Z]+')} {str_extract(bill_number, '[0-9]+')}")
    ) 
  }
  
  text_files <- get_file_path(session, bill_number)
  # Download text files
  downloaded_files <- sapply(text_files, function(file){download_bill_text(UUID, file)}) |> as.character()
  
  # Process text files
  processed_files <- lapply(downloaded_files, function(file){process_bill_text(UUID, session, file)})
  
}

# Build list of UUIDs
vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
gs_tx_list <- googlesheets4::read_sheet('1k2BT8QfSLm47n_vTf6yDoaoeq0zxDyXQ9ExGTSw7M_c') |> janitor::clean_names()

gs_tx_list <- gs_tx_list |> 
  mutate(
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number_formatted = str_extract(bill_number, "[0-9]+$"),
    bill_type = case_match(
      bill_type,
      "SB" ~ "S",
      "HB" ~ "H",
      .default = bill_type
    ),
    year = case_match(
      session,
      "74R" ~ "1995",
      "74R" ~ "1996",
      "75R" ~ "1997",
      "75R" ~ "1998",
      "76R" ~ "1999",
      "76R" ~ "2000"
    ),
    UUID = glue("TX{year}{bill_type}{bill_number_formatted}")
  ) |> pull(UUID)

tx_master <- vrleg_master_file |> filter(STATE == 'TX' & YEAR %in% c(1995:2014)) |> pull(UUID)

already_processed <- dir_ls('/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/texas') |> basename()
bills_to_process <- c(gs_tx_list, tx_master) |> unique() |> setdiff(already_processed)

future_map(bills_to_process, scrape_text)
