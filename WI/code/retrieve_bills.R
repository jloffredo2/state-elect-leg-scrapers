library(tidyverse)
library(glue)
library(googlesheets4)

old_bill_list <- read_sheet("1MR4o6IQR93YYoybx6H8GjCfhSbyM4ePzf3Le0V7Ssk8") |>
  janitor::clean_names() |>
  mutate(
    bill_type = str_extract(bill_number, "^[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "AB" ~ "A",
      "SB" ~ "S",
    ),
    bill_number_uuid = str_extract(bill_number, "[0-9]+$"),
    UUID = glue("WI{session}{bill_type}{bill_number_uuid}"),
    bill_number = str_to_lower(bill_number) |> str_to_upper(),
    session = as.character(session)
  ) |>
  select(UUID, session, bill_number)

vrleg_master_file <- readRDS("~/Desktop/GitHub/election-roll-call/bills/vrleg_master_file.rds")
master <- vrleg_master_file |> 
  filter(STATE == 'WI' & YEAR %in% c(1995:2014)) |>
  mutate(
    bill_id = str_remove_all(UUID, "WI"),
    year = str_extract(bill_id, "^[0-9]{4}"),
    session = case_when(
      year %in% c("1995", "1996") ~ "1995",
      year %in% c("1997", "1998") ~ "1997",
      year %in% c("1999", "2000") ~ "1999",
      year %in% c("2001", "2002") ~ "2001",
      year %in% c("2003", "2004") ~ "2003",
      year %in% c("2005", "2006") ~ "2005",
      year %in% c("2007", "2008") ~ "2007",
      year %in% c("2009", "2010") ~ "2009",
      year %in% c("2011", "2012") ~ "2011",
      year %in% c("2013", "2014") ~ "2013"
    ),
    bill_type = str_extract(bill_id, "[A-Z]+"),
    bill_type = case_match(
      bill_type,
      "A" ~ "AB",
      "S" ~ "SB",
      .default = bill_type,
    ),
    bill_number = str_extract(bill_id, "[0-9]+$"),
    bill_number = glue("{bill_type}{bill_number}")
  ) |>
  select(UUID, session, bill_number) |>
  bind_rows(old_bill_list)

write_csv(master, "WI/output/WI_bills_to_process.csv")
