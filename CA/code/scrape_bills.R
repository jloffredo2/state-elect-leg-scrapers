##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape CA
## Date: February 2025
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(jsonlite)
library(glue)
library(rvest)
library(furrr)

plan(multisession, workers = 11)

OUTPUT_PATH <- 'CA/output'
ca_roll_call <- readRDS('~/Dropbox (MIT)/previous_leg_files/CA/ca_roll_call.rds')

# Functions ---------------------------------------------------------------
safe_call <- function(expr, label) {
  tryCatch(
    expr,
    error = function(e) {
      message(glue::glue("Error in {label}: {e$message}"))
      return(NULL)
    }
  )
}

# Format bill base URL
build_url <- function(session, bill_number){
  # Transform session identifier to match URL format
  session_id <- case_match(session,
    '2013-2014' ~ '1314',
    '2011-2012' ~ '1112',
    '2009-2010' ~ '0910',
    '2007-2008' ~ '0708',
    '2005-2006' ~ '0506',
    '2003-2004' ~ '0304',
    '2001-2002' ~ '0102',
    '1999-2000' ~ '9900',
    '1997-1998' ~ '9798',
    '1995-1996' ~ '9596'
  )
  # Transform bill number to match URL format
  bill_id <- str_replace_all(bill_number, ' ', '_') |> str_to_lower()

  return(glue('http://leginfo.ca.gov/cgi-bin/postquery?bill_number={bill_id}&sess={session_id}'))
}

# Retrieve all available page links
get_page_links <- function(state_url){
  # Scrape main bill page
  page <- read_html(state_url)
  # Collect all hyperlinks
  page_links <- page |> html_nodes("a") |> html_attr("href")
  page_links <- glue("http://leginfo.ca.gov{page_links}")

  return(page_links)
}

# Get bill metadata
get_bill_metadata <- function(uuid, session, bill_number, state_url, page_links){
  OUTPUT_PATH <- 'CA/output'
  # Scrape bill status page
  status_url <- page_links |> str_subset("_status")
  status_page_text <- read_html(status_url) |> html_text()
  
  title <- sub(".*TITLE\\t:\\s*(.*)\\n.*", "\\1", status_page_text)
  title <- gsub("[\n\t]", " ", title) |> str_trim() |> str_squish()
  
  lines <- str_split(status_page_text, "\n")[[1]]
  
  # Extract status
  last_hist_action_index <- grep("LAST HIST. ACTION", lines)
  status_lines <- lines[last_hist_action_index:length(lines)]
  ## Capture all lines until we find "TITLE"
  status_text <- status_lines[1]
  for (i in 2:length(status_lines)) {
    if (str_detect(status_lines[i], "TITLE")) break
    status_text <- str_c(status_text, " ", status_lines[i])
  }
  status <- str_remove(status_text, "^LAST HIST. ACTION") |> str_trim() |> str_squish() |> str_remove("^:") |> str_trim()
  status <- ifelse(str_detect(status, "Chaptered"), glue("Enacted - {status}"), status)
  
  # Extract topic
  topic_index <- grep("TOPIC", lines)
  topic_lines <- lines[topic_index:length(lines)]
  ## Capture all lines until we find "+LAST AMENDED"
  topic_text <- topic_lines[1]
  for (i in 2:length(topic_lines)) {
    if (str_detect(topic_lines[i], "LAST AMENDED")) break
    topic_text <- str_c(topic_text, " ", topic_lines[i])
  }
  topic <- str_remove(topic_text, "^TOPIC") |> str_trim() |> str_squish() |> str_remove("^:") |> str_trim()
  
  tibble(
    uuid = uuid, 
    state = 'CA', 
    session = session, 
    state_bill_id = bill_number, 
    title = title, 
    description = topic, 
    status = status, 
    state_url = state_url
  ) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/bill_metadata/{uuid}.json"))
}

# Get bill sponsors
get_bill_sponsors <- function(uuid, session, bill_number, page_links){
  OUTPUT_PATH <- 'CA/output'
  # Scrape bill status page
  status_url <- page_links |> str_subset("_status")
  status_page_text <- read_html(status_url) |> html_text()
  
  # Extract status using regex
  # Split the text into lines
  lines <- str_split(status_page_text, "\n")[[1]]
  authors_index <- grep("AUTHOR", lines)
  author_lines <- lines[authors_index:length(lines)]
  # Capture all lines until we find "TITLE"
  author_text <- author_lines[1]
  for (i in 2:length(author_lines)) {
    if (str_detect(author_lines[i], "TOPIC")) break
    author_text <- str_c(author_text, " ", author_lines[i])
  }
  authors <- str_remove(author_text, "^AUTHOR\\(S\\)\t:") |> str_trim() |> str_squish() |> str_remove("^:") |> str_trim()
  sponsors <- authors |>
    str_extract("^[^(]+") |>
    str_remove("[\\s.]+$") |>
    str_replace_all("\\band\\b", ",") |>
    str_split(",\\s*") |>
    (\(x) x[[1]])() |>
    str_trim()
  sponsors <- sponsors[nzchar(sponsors)]
  
  ## Cosponsors: contents of the first (...) with multiple format fallbacks
  paren <- str_match(authors, "\\(([^)]*)\\)")[, 2]
  
  if (!is.na(paren)) {
    cosponsors <- paren |>
      # drop possible titles/prefixes and leading "Coauthors:" forms
      str_remove("(?i)^\\s*(senators?|reps?|representatives?|members)[:]?\\s*") |>
      str_remove("(?i)^\\s*coauthors?[:]?\\s*") |>
      # drop trailing ", coauthor(s)." forms
      str_remove(",?\\s*(?i)coauthors?\\.?$") |>
      # normalize "and" to commas, then split
      str_replace_all("\\band\\b", ",") |>
      str_split(",\\s*") |>
      (\(x) x[[1]])() |>
      str_trim()
    cosponsors <- cosponsors[nzchar(cosponsors)]
  } else {
    cosponsors <- character(0)
  }
  
  tibble(
    uuid = uuid, 
    state = 'CA', 
    session = session, 
    state_bill_id = bill_number, 
    sponsor_name = c(sponsors, cosponsors),
    sponsor_type = c(rep('sponsor', length(sponsors)), rep('cosponsor', length(cosponsors)))
  ) |> 
    group_by(uuid, state, session, state_bill_id) |>
    nest(sponsors = c(sponsor_name, sponsor_type)) |>
    ungroup() |>
    as.list() |>
    toJSON(pretty = T,auto_unbox = T) |> 
    writeLines(glue("{OUTPUT_PATH}/sponsors/{uuid}.json"))
  
}

get_bill_history <- function(uuid, session, bill_number, page_links){
  OUTPUT_PATH <- 'CA/output'
  # Scrape bill history page
  history_url <- page_links |> str_subset("_history")
  history_page_text <- read_html(history_url) |> html_text() |> str_remove_all("COMPLETE BILL HISTORY")
  
  # Extract the section after "BILL HISTORY"
  lines <- str_split(history_page_text, "\n")[[1]]
  history_index <- grep("BILL HISTORY", lines)
  history_lines <- lines[history_index:length(lines)]
  
  # Separate the date and action
  history_df <- data.frame(
    date = str_extract(history_lines, "^[A-Za-z\\.]+\\s+\\d{1,2}"),
    action = str_replace(history_lines, "^[A-Za-z]+\\s+\\d{1,2}\\s*", "")
  ) |>
    filter(action != 'BILL HISTORY') |>
    mutate(action = case_when(
      !is.na(date) ~ str_replace(action, date, "") |> str_trim() |> str_squish(),
      TRUE ~ action)
    ) |>
    mutate(action = if_else(is.na(date), lag(action, default = "") %>% paste(action, sep = " "), action)) |>
    fill(date) |>
    group_by(date) |>
    filter(nchar(action) == max(nchar(action), na.rm = TRUE)) |>
    ungroup() |>
    mutate(year = ifelse(is.na(date) & str_detect(action,"[0-9]{4}"), action, NA_character_) |> as.integer()) |> 
    fill(year) |>
    mutate(
      date = glue("{date}, {year}") |> mdy(),
      action = str_remove_all(action,"[\n\t]")) |>
    filter(!is.na(date)) |>
    select(-year)
  
  tibble(
    uuid = uuid, 
    state = 'CA', 
    session = session, 
    state_bill_id = bill_number, 
    date = history_df$date,
    action = history_df$action
  ) |> 
    group_by(uuid, state, session, state_bill_id) |>
    nest(history = c(date, action)) |>
    ungroup() |>
    as.list() |>
    toJSON(pretty = T,auto_unbox = T) |> 
    writeLines(glue("{OUTPUT_PATH}/bill_history/{uuid}.json"))
}

get_votes <- function(uuid, session, bill_number){
  OUTPUT_PATH <- 'CA/output'
  # Retrieve votes
  output_votes <- ca_roll_call |> filter(session == !!session, state_bill_id == bill_number) |>
    mutate(uuid = uuid) |> 
    select(uuid, everything())
  
  if(nrow(output_votes) == 1){
    vote_date <- output_votes |> pull(date) |> as.character() |> str_replace_all("-","")
    output_votes |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/votes/{uuid}_{vote_date}.json"))
  } else if(nrow(output_votes) > 1) {
    for(i in 1:nrow(output_votes)){
      vote_date = output_votes[i,] |> pull(date) |> as.character() |> str_replace_all("-","")
      output_votes[i,] |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/votes/{uuid}_{vote_date}_{i}.json"))
    }
  }
}

scrape_bill <- function(UUID, session = NA, bill_number = NA){
  message(UUID)
  year <- str_extract(UUID, "[0-9]{4}")
  
  if(is.na(session)){
    session <- case_match(
      year,
      "2014" ~ "2013-2014",
      "2013" ~ "2013-2014",
      "2012" ~ "2011-2012",
      "2011" ~ "2011-2012",
      "2010" ~ "2009-2010",
      "2009" ~ "2009-2010",
      "2008" ~ "2007-2008",
      "2007" ~ "2007-2008",
      "2006" ~ "2005-2006",
      "2005" ~ "2005-2006",
      "2004" ~ "2003-2004",
      "2003" ~ "2003-2004",
      "2002" ~ "2001-2002",
      "2001" ~ "2001-2002",
      "2000" ~ "1999-2000",
      "1999" ~ "1999-2000",
      "1998" ~ "1997-1998",
      "1997" ~ "1997-1998",
      "1996" ~ "1995-1996",
      "1995" ~ "1995-1996"
    )
  }
  
  if(is.na(bill_number)){
    bill_number <- str_remove_all(UUID, "^CA[0-9]{4}")
    bill_number <- case_when(
      str_detect(bill_number, "^S[0-9]") ~ glue("SB {str_remove(bill_number, 'S')}"),
      str_detect(bill_number, "^A[0-9]") ~ glue("AB {str_remove(bill_number, 'A')}"),
      TRUE ~ glue("{str_extract(bill_number, '^[A-Z]+')} {str_extract(bill_number, '[0-9]+')}")
    ) 
  }
  
  state_url <- build_url(session, bill_number)
  page_links <- get_page_links(state_url)
  
  safe_call(get_bill_metadata(UUID, session, bill_number, state_url, page_links), "get_bill_metadata")
  safe_call(get_bill_sponsors(UUID, session, bill_number, page_links), "get_bill_sponsors")
  safe_call(get_bill_history(UUID, session, bill_number, page_links), "get_bill_history")
  safe_call(get_votes(UUID, session, bill_number), "get_votes")
}

# Process bill list -------------------------------------------------------
vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")

gs_list <- googlesheets4::read_sheet('1aLYLTXjLK7jKMi04_PgWbN9bSMdQxLVdLasWURcbu4s') |> janitor::clean_names()
gs_list <- gs_list |> 
  mutate(
    bill_number = str_replace(bill_number, "\\s",""),
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number = str_extract(bill_number, "[0-9]+$"),
    bill_type_uuid = case_match(
      bill_type,
      "AB" ~ "A",
      "SB" ~ "S",
      .default = bill_type
    ),
    year = str_extract(session, "^[0-9]{4}"),
    UUID = glue("CA{year}{bill_type_uuid}{bill_number}"),
    bill_number = glue("{bill_type} {bill_number}")
  ) |>
  select(UUID, session, bill_number) 

ca_master <- vrleg_master_file |> 
  filter(STATE == 'CA' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "CA"),
    session = str_extract(bill_id, "^[0-9]{4}"),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "A" ~ "AB",
      "S" ~ "SB",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    session = case_when(
      session %in% c('1995','1996') ~ '1995-1996',
      session %in% c('1997','1998') ~ '1997-1998',
      session %in% c('1999','2000') ~ '1999-2000',
      session %in% c('2001','2002') ~ '2001-2002',
      session %in% c('2003','2004') ~ '2003-2004',
      session %in% c('2005','2006') ~ '2005-2006',
      session %in% c('2007','2008') ~ '2007-2008',
      session %in% c('2009','2010') ~ '2009-2010',
      session %in% c('2011','2012') ~ '2011-2012',
      session %in% c('2013','2014') ~ '2013-2014'
    ),
    bill_number = glue("{bill_type} {bill_number}")
  ) |>
  select(UUID, session, bill_number)

bills_to_process <- bind_rows(ca_master,gs_list) |> distinct()

bills_to_process |> future_pmap(scrape_bill)

