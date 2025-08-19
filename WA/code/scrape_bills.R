##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Scrape WA
## Date: April 2025
## Author: Joe Loffredo
##################################################

library(tidyverse)
library(jsonlite)
library(glue)
library(httr)
library(xml2)

rm(list = ls())
gc()

OUTPUT_PATH <- 'WA/output'

# Functions
build_url <- function(session, bill_number){
  session <- str_sub(session,1,4)
  glue("https://app.leg.wa.gov/billsummary?BillNumber={bill_number}&Year={session}#rollCallPopup")
}

get_bill_metadata <- function(UUID, session, bill_number, output_path){
  # Build legislative info site url
  state_url <- build_url(session, bill_number)
  
  # Extract metadata from GetLegislation
  payload <- list(Biennium = session, billNumber = bill_number)
  response <- POST(url = "https://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislation", body = payload, encode = "form")
  
  if (status_code(response) == 200) {
    # Parse the response content
    metadata <- content(response, as = "parsed")
    legislation_nodes <- xml_find_all(metadata, "//d1:ArrayOfLegislation/d1:Legislation", ns = xml_ns(metadata))
    metadata <- as_list(legislation_nodes)
    ## Get latest version
    metadata <- metadata[[length(metadata)]]
    ## Get values
    title <- metadata$LegalTitle |> unlist()
    description <- metadata$LongDescription |> unlist()
    status <- metadata$CurrentStatus$HistoryLine |> unlist()
    status <- ifelse(str_detect(status,"Effective|Chapter"), glue("Enacted - {status}"), status)
    
    tibble(
      uuid = UUID, 
      state = 'WA', 
      session = session, 
      state_bill_id = bill_number, 
      title = title,
      description = description,
      status = status,
      state_url = state_url
    ) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{output_path}/bill_metadata/{UUID}.json"))
  } else {
    message("error getting bill metadata")
    return(NULL)
  }
  
}

get_sponsors <- function(UUID, session, bill_number, output_path){
  # Extract metadata from GetSponsors
  payload <- list(biennium = session, billId = bill_number)
  response <- POST(url = "https://wslwebservices.leg.wa.gov/LegislationService.asmx/GetSponsors", body = payload, encode = "form")
  
  if (status_code(response) == 200) {
    sponsors <- content(response, as = "parsed")
    sponsors_nodes <- xml_find_all(sponsors, "//d1:ArrayOfSponsor/d1:Sponsor", ns = xml_ns(sponsors))
    sponsors <- as_list(sponsors_nodes)
    
    flatten_sponsor <- function(sponsor) {
      list(
        Id = sponsor$Id[[1]],
        Name = sponsor$Name[[1]],
        LongName = sponsor$LongName[[1]],
        Agency = sponsor$Agency[[1]],
        Acronym = sponsor$Acronym[[1]],
        Type = sponsor$Type[[1]],
        Order = sponsor$Order[[1]],
        FirstName = sponsor$FirstName[[1]],
        LastName = sponsor$LastName[[1]]
      )
    }
    
    sponsors_df <- map_dfr(sponsors, flatten_sponsor) |> mutate(Type = ifelse(Type == 'Primary', 'sponsor', 'cosponsor'))
    
    tibble(
      uuid = UUID, 
      state = 'WA', 
      session = session, 
      state_bill_id = bill_number, 
      sponsor_name = sponsors_df$Name,
      sponsor_type = sponsors_df$Type
    ) |> 
      group_by(uuid, state, session, state_bill_id) |>
      nest(sponsors = c(sponsor_name, sponsor_type)) |>
      ungroup() |>
      as.list() |>
      toJSON(pretty = T,auto_unbox = T) |> 
      writeLines(glue("{output_path}/sponsors/{UUID}.json"))
    } else {
    message("error getting sponsors")
    return(NULL)
  }
  
}

get_bill_history <- function(UUID, session, bill_number, output_path){
  message(UUID)
  begin_date <- switch(
    session,
    '1995-96' = '1995-01-01',
    '1997-98' = '1997-01-01',
    '1999-00' = '1999-01-01',
    '2001-02' = '2001-01-01',
    '2003-04' = '2003-01-01',
    '2005-06' = '2005-01-01',
    '2007-08' = '2007-01-01',
    '2009-10' = '2009-01-01',
    '2011-12' = '2011-01-01',
    '2013-14' = '2013-01-01',
    '2015-16' = '2015-01-01',
    '2017-18' = '2017-01-01',
    '2019-20' = '2019-01-01',                   
    '2021-22' = '2021-01-01',
    '2023-24' = '2023-01-01'
  )
  
  end_date <- switch(
    session,
    '1995-96' = '1996-12-31',
    '1997-98' = '1998-12-31',
    '1999-00' = '2000-12-31',
    '2001-02' = '2002-12-31',
    '2003-04' = '2004-12-31',
    '2005-06' = '2006-12-31',
    '2007-08' = '2008-12-31',
    '2009-10' = '2010-12-31',
    '2011-12' = '2012-12-31',
    '2013-14' = '2014-12-31',
    '2015-16' = '2016-12-31',
    '2017-18' = '2018-12-31',
    '2019-20' = '2020-12-31',                   
    '2021-22' = '2022-12-31',
    '2023-24' = '2024-12-31'
  )
  
  # Extract metadata from GetLegislation
  payload <- list(biennium = session, billNumber = bill_number, beginDate = begin_date, endDate = end_date)
  response <- POST(url = "https://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislativeStatusChangesByBillNumber", body = payload, encode = "form")
  
  if (status_code(response) == 200) {
    history <- content(response, as = "parsed")
    history_nodes <- xml_find_all(history, "//d1:ArrayOfLegislativeStatus/d1:LegislativeStatus", ns = xml_ns(history))
    history <- as_list(history_nodes)
    
    if(is_empty(history)) {
      message("No history found for this bill")
      return(NULL)
    }
    
    flatten_history <- function(history) {
      list(
        BillId = history$BillId[[1]],
        HistoryLine = history$HistoryLine[[1]],
        ActionDate = history$ActionDate[[1]],
        AmendedByOppositeBody = history$AmendedByOppositeBody[[1]],
        PartialVeto = history$PartialVeto[[1]],
        Veto = history$Veto[[1]],
        AmendmentsExist = history$AmendmentsExist[[1]],
        Status = history$Status[[1]]
      )
    }
    
    history_df <- map_dfr(history, flatten_history) |>
      mutate(ActionDate = as_date(ActionDate))
    
    tibble(
      uuid = UUID, 
      state = 'WA', 
      session = session, 
      state_bill_id = bill_number, 
      date = history_df$ActionDate,
      action = history_df$HistoryLine
    ) |> 
      group_by(uuid, state, session, state_bill_id) |>
      nest(history = c(date, action)) |>
      ungroup() |>
      as.list() |>
      toJSON(pretty = T,auto_unbox = T) |> 
      writeLines(glue("{output_path}/bill_history/{UUID}.json"))
  } else {
    message("error getting bill history")
    return(NULL)
  }
}

get_votes <- function(UUID, session, bill_number, output_path){
  # Extract metadata from GetRollCalls
  payload <- list(biennium = session, billNumber = bill_number)
  response <- POST(url = "https://wslwebservices.leg.wa.gov/LegislationService.asmx/GetRollCalls", body = payload, encode = "form")
  
  if (status_code(response) == 200) {
    votes <- content(response, as = "parsed")
    votes_nodes <- xml_find_all(votes, "//d1:ArrayOfRollCall/d1:RollCall", ns = xml_ns(votes))
    votes <- as_list(votes_nodes)
    
    if(is_empty(votes)) {
      message("No votes found for this bill")
      return(NULL)
    }
    
    flatten_votes <- function(votes) {
      metadata <- list(
        Agency = votes$Agency[[1]] %||% NA,
        BillId = votes$BillId[[1]] %||% NA,
        Biennium = votes$Biennium[[1]] %||% NA,
        Motion = votes$Motion[[1]] %||% NA,
        SequenceNumber = votes$SequenceNumber[[1]] %||% NA,
        VoteDate = votes$VoteDate[[1]] %||% NA,
        YeaVotes = as.numeric(votes$YeaVotes$Count[[1]]) %||% NA,
        NayVotes = as.numeric(votes$NayVotes$Count[[1]]) %||% NA,
        AbsentVotes = as.numeric(votes$AbsentVotes$Count[[1]]) %||% NA,
        ExcusedVotes = as.numeric(votes$ExcusedVotes$Count[[1]]) %||% NA
      )
      # extract member-level votes
      member_votes <- map_dfr(votes$Votes, function(v) {
        tibble(
          Name = v$Name,
          Vote = v$VOte
        )
      }) |>
        mutate(
          Vote = case_when(
            Vote == 'Yea' ~ 'Yea',
            Vote == 'Nay' ~ 'Nay',
            Vote == 'Absent' ~ 'Absent',
            TRUE ~ 'NV'
          )
        ) |>
        rename(name = Name, response = Vote)
      
      # add roll_call as list-column
      metadata$roll_call <- list(member_votes)
      return(metadata)
    }
    
    votes_df <- map_dfr(votes, flatten_votes) |>
      mutate(
        uuid = UUID,
        state = 'WA',
        session = session,
        state_bill_id = bill_number,
        chamber = case_match(
          Agency,
          'Senate' ~ 'S',
          'House' ~ 'H',
          .default = NA_character_
        ),
        date = as_date(VoteDate),
        description = Motion,
        yeas = as.integer(YeaVotes),
        nays = as.integer(NayVotes),
        other = as.integer(AbsentVotes) + as.integer(ExcusedVotes),
      ) |>
      select(uuid, state, session, state_bill_id, chamber, date, description, yeas, nays, other, roll_call)
    
    for(i in 1:nrow(votes_df)){
      votes_df |>
        slice(i) |>
        as.list() |>
        toJSON(pretty = T, auto_unbox = T) |>
        writeLines(glue("{output_path}/votes/{UUID}_{i}.json"))
    }
  } else {
    message("error getting votes")
    return(NULL)
  }
}

scrape_bill <- function(UUID, session = NA, bill_number = NA, output_path){
  get_bill_metadata(UUID, session, bill_number, output_path)
  get_sponsors(UUID, session, bill_number, output_path)
  get_bill_history(UUID, session, bill_number, output_path)
  get_votes(UUID, session, bill_number, output_path)
}

# Process bill list -------------------------------------------------------
vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")

gs_list <- googlesheets4::read_sheet('1jzW_WzyAAEFKxTGZeSmyIn9UQpXhHqsEb5zCBmni228') |> janitor::clean_names()
gs_list <- gs_list |> 
  mutate(
    bill_number = str_replace(bill_number, "\\s",""),
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_number = str_extract(bill_number, "[0-9]+$"),
    bill_type_uuid = case_match(
      bill_type,
      "HB" ~ "H",
      "SB" ~ "S",
      .default = bill_type
    ),
    UUID = glue("WA{year}{bill_type_uuid}{bill_number}"),
    session = case_match(
      session,
      "1995-1996" ~ "1995-96",
      "1997-1998" ~ "1997-98",
      "1999-2000" ~ "1999-00",
      "2001-2002" ~ "2001-02",
      "2003-2004" ~ "2003-04",
      "2005-2006" ~ "2005-06",
      "2007-2008" ~ "2007-08",
      "2009-2010" ~ "2009-10",
      "2011-2012" ~ "2011-12",
      "2013-2014" ~ "2013-14"
    )
  ) |>
  select(UUID, session, bill_number) 

wa_master <- vrleg_master_file |> 
  filter(STATE == 'WA' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "WA"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_match(
      year,
      "1995" ~ "1995-96",
      "1996" ~ "1995-96",
      "1997" ~ "1997-98",
      "1998" ~ "1997-98",
      "1999" ~ "1999-00",
      "2000" ~ "1999-00",
      "2001" ~ "2001-02",
      "2002" ~ "2001-02",
      "2003" ~ "2003-04",
      "2004" ~ "2003-04",
      "2005" ~ "2005-06",
      "2006" ~ "2005-06",
      "2007" ~ "2007-08",
      "2008" ~ "2007-08",
      "2009" ~ "2009-10",
      "2010" ~ "2009-10",
      "2011" ~ "2011-12",
      "2012" ~ "2011-12",
      "2013" ~ "2013-14",
      "2014" ~ "2013-14"
    ),
    bill_number = str_extract(bill_id, "[0-9]+$")
  ) |>
  select(UUID, session, bill_number)

bills_to_process <- bind_rows(wa_master,gs_list) |> distinct()

# Define output path
OUTPUT_PATH <- 'WA/output'

# Make sure output directories exist
dir.create(file.path(OUTPUT_PATH, "bill_metadata"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "sponsors"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PATH, "bill_history"), recursive = TRUE, showWarnings = FALSE)

# Add output_path column to the dataframe
bills_to_process <- bills_to_process |> 
  mutate(output_path = OUTPUT_PATH)

# Run the scraper with all parameters
bills_to_process |> future_pmap(scrape_bill)

