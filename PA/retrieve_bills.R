library(tidyverse)
library(glue)
library(jsonlite)

old_session_bills <- fromJSON("PA/pennsylvania_sessions_ids.json") |> janitor::clean_names() |>
  mutate(
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_type_uuid = case_match(
      bill_type,
      "HB" ~ "H",
      "SB" ~ "S",
      .default = bill_type
    ),
    bill_number_uuid = str_extract(bill_number, "[0-9]+$"),
    year = str_extract(session, "^[0-9]{4}"),
    UUID = glue("PA{year}{bill_type_uuid}{bill_number_uuid}")
  ) |>
  select(UUID, session, bill_number)

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
pa_master <- vrleg_master_file |> 
  filter(STATE == 'PA' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, 'PA'),
    session = str_extract(bill_id, "^[0-9]{4}"),
    session = case_match(
      session,
      "2001" ~ "2001-2002 Regular Session",
      "2002" ~ "2001-2002 Regular Session",
      "2003" ~ "2003-2004 Regular Session",
      "2004" ~ "2003-2004 Regular Session",
      "2005" ~ "2005-2006 Regular Session",
      "2006" ~ "2005-2006 Regular Session",
      "2007" ~ "2007-2008 Regular Session",
      "2008" ~ "2007-2008 Regular Session",
      "2009" ~ "2009-2010 Regular Session",
      "2010" ~ "2009-2010 Regular Session",
      "2011" ~ "2011-2012 Regular Session",
      "2012" ~ "2011-2012 Regular Session",
      "2013" ~ "2013-2014 Regular Session",
      "2014" ~ "2013-2014 Regular Session"
    ), 
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "H" ~ "HB",
      "S" ~ "SB",
      .default = bill_type
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type} {bill_number}")
  ) |>
  select(UUID, session, bill_number)

bills_to_process <- bind_rows(pa_master,old_session_bills) |> distinct()

write_csv(bills_to_process, "PA/output/PA_bills_to_process.csv")
