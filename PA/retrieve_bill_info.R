##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Collect PA Data Downloads
## Date: July 2025
## Author: Joe Loffredo
##################################################

library(tidyverse)
library(xml2)
library(lubridate)
library(glue)
library(furrr)

plan(multisession, workers = 11)

parse_pa_xml <- function(file_path) {
  message(glue("Processing: {basename(file_path)}"))
  xml_data <- read_xml(file_path)
  bills <- xml_find_all(xml_data, "//bill")
  
  bills_clean <- map_dfr(bills, function(bill) {
    tibble(
      bill_id = xml_attr(bill, "id"),
      session_year = as.numeric(xml_text(xml_find_first(bill, "sessionYear"))),
      session = as.numeric(xml_text(xml_find_first(bill, "session"))),
      body = xml_text(xml_find_first(bill, "body")),
      type = xml_text(xml_find_first(bill, "type")),
      type_description = xml_attr(xml_find_first(bill, "type"), "description"),
      sub_type = xml_text(xml_find_first(bill, "subType")),
      number = str_pad(as.numeric(xml_text(xml_find_first(bill, "number"))), 4, pad = "0"),
      short_title = str_trim(xml_text(xml_find_first(bill, "shortTitle")))
    )
  })
  
  sponsors_clean <- map_dfr(bills, function(bill) {
    bill_id <- xml_attr(bill, "id")
    xml_find_all(bill, "sponsors/sponsor") |>
      map_dfr(function(sponsor) {
        tibble(
          bill_id = bill_id,
          sequence_number = as.numeric(xml_attr(sponsor, "sequenceNumber")),
          fill_sequence = as.numeric(xml_attr(sponsor, "fillSequence")),
          party = factor(xml_attr(sponsor, "party"), levels = c("D", "R")),
          sponsor_body = factor(xml_attr(sponsor, "body"), levels = c("H", "S")),
          district_number = as.numeric(xml_attr(sponsor, "distictNumber")),
          sponsor_name = str_trim(xml_text(sponsor))
        )
      })
  }) |>
    arrange(bill_id, sequence_number)
  
  printer_numbers_clean <- map_dfr(bills, function(bill) {
    bill_id <- xml_attr(bill, "id")
    xml_find_all(bill, "printersNumberHistory/number") |>
      map_dfr(function(pn) {
        tibble(
          bill_id = bill_id,
          sequence = as.numeric(xml_attr(pn, "sequence")),
          printer_number = str_pad(xml_text(pn), 4, pad = "0")
        )
      })
  }) |>
    arrange(bill_id, sequence)
  
  actions_clean <- map_dfr(bills, function(bill) {
    bill_id <- xml_attr(bill, "id")
    xml_find_all(bill, "actionHistory/action") |>
      map_dfr(function(action) {
        date_text <- xml_text(xml_find_first(action, "date"))
        parsed_date <- case_when(
          date_text == "" ~ NA_Date_,
          str_detect(date_text, "^\\d{2}/\\d{2}/\\d{2}$") ~ {
            yr <- as.numeric(str_sub(date_text, 7, 8))
            full_yr <- ifelse(yr > 50, 1900 + yr, 2000 + yr)
            as.Date(paste0(str_sub(date_text, 1, 6), full_yr), format = "%m/%d/%Y")
          },
          TRUE ~ as.Date(date_text, format = "%m/%d/%Y")
        )
        
        tibble(
          bill_id = bill_id,
          action_sequence = as.numeric(xml_attr(action, "sequence")),
          action_chamber = factor(xml_attr(action, "actionChamber"), levels = c("H", "S", "E")),
          verb = xml_text(xml_find_first(action, "verb")),
          committee = na_if(xml_text(xml_find_first(action, "committee")), ""),
          date = parsed_date,
          printer_number = xml_text(xml_find_first(action, "printersNumber")),
          roll_call_vote = na_if(xml_text(xml_find_first(action, "rollCallVote")), ""),
          full_action = xml_text(xml_find_first(action, "fullAction"))
        )
      })
  }) |>
    arrange(bill_id, action_sequence)
  
  list(
    bills = bills_clean,
    sponsors = sponsors_clean,
    printer_numbers = printer_numbers_clean,
    actions = actions_clean
  )
}

process_all_pa_files <- function(xml_dir) {
  xml_files <- list.files(xml_dir, pattern = "\\.xml$", full.names = TRUE)
  if (length(xml_files) == 0) stop("No XML files found.")
  future_map(xml_files, parse_pa_xml, .progress = TRUE)
}

results <- process_all_pa_files('raw_files')

pa_data <- list(
  bills = map_dfr(results, "bills"),
  sponsors = map_dfr(results, "sponsors"),
  printer_numbers = map_dfr(results, "printer_numbers"),
  actions = map_dfr(results, "actions")
)

write_csv(pa_data$bills, "bills.csv")
write_csv(pa_data$sponsors, "sponsors.csv")
write_csv(pa_data$printer_numbers, "printer_numbers.csv")
write_csv(pa_data$actions, "actions.csv")
