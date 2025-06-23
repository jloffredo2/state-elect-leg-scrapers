##################################################
## Project: State Election Legislation Scrapers
## Script purpose: Retrieve SD Links
## Date: June 2025
## Author: Joe Loffredo
##################################################

# produces urls from csv bill list for each session (e.g., https://sdlegislature.gov/Session/Bills/20)
library(tidyverse)

sd_files <- list.files(path = "text/state-scrapers",pattern = "South Dakota", full.names = TRUE)

process_files <- function(file){
  year = str_extract(file, "\\d{4}")
  
  read_csv(file, col_types = cols(.default = "c")) %>%
    mutate(year = year) %>%
    select(year, everything())
}

map_dfr(sd_files, process_files) |>
  janitor::clean_names() |>
  select(year, state_bill_number = combined, state_bill_id = bill_id) |>
  write_csv("text/state-scrapers/sd_bill_links.csv")
