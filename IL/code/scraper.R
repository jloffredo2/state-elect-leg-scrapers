library(tidyverse)
library(jsonlite)
library(glue)
library(furrr)

plan(multisession, workers = 11)

rm(list = ls())
gc()

# Dropbox path for Illinois data
DROPBOX_PATH <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/previous_leg_files/IL'  # Change this to where your Illinois files are stored
OUTPUT_PATH <- 'IL/output'  # Output directory for Illinois data

# Wrangle OpenStates ------------------------------------------------------
#### Sponsors ####
sponsorships <- lapply(list.files(path = DROPBOX_PATH, pattern = "bill_sponsorships.csv", full.names = T, recursive = T), read_csv)
sponsorships_df <- do.call(bind_rows,sponsorships)

#### Bills ####
bills <- lapply(list.files(path = DROPBOX_PATH, pattern = "bills.csv",full.names = T, recursive = T), read_csv) 
bills_df <- do.call(bind_rows,bills)

#### Vote Counts ####
vote_counts <- lapply(list.files(path = DROPBOX_PATH, pattern = "vote_counts.csv",full.names = T, recursive = T), read.csv)
vote_counts_df <- do.call(rbind,vote_counts)

#### Vote Events ####
votes <- lapply(list.files(path = DROPBOX_PATH, pattern = "votes.csv",full.names = T, recursive = T), read.csv)
votes_df <- do.call(rbind,votes)

#### Bill Actions ####
bill_actions <- lapply(list.files(path = DROPBOX_PATH, pattern = "bill_actions.csv",full.names = T, recursive = T), read.csv)
bill_actions_df <- do.call(rbind,bill_actions)

#### Vote Roll Calls ####
vote_people <- lapply(list.files(path = DROPBOX_PATH, pattern = "vote_people.csv",full.names = T, recursive = T), read.csv)
vote_people_df <- do.call(rbind,vote_people)

# Clean lookup files ------------------------------------------------------
# State name --> state code
bills_df$jurisdiction <- state.abb[match(bills_df$jurisdiction,state.name)]

# Get most recent action and intro date
bill_actions_df$date <- lubridate::ymd(as.Date(bill_actions_df$date))
most_recent_action <- bill_actions_df |>
  group_by(bill_id) |>
  slice(which.max(order)) |>
  mutate(
    last_action_date = date,
    last_action_description = case_when(
      classification == "['became-law']" ~ glue("Enacted - {description}"),
      str_detect(description, "Public Act") ~ glue("Enacted - {description}"),
      TRUE ~ description
    )) |>
  select(bill_id, last_action_description, last_action_date) |>
  distinct()

intro_date <- bill_actions_df |> 
  group_by(bill_id) |>
  slice(which.min(order)) |>
  mutate(intro_year = year(date)) |>
  select(bill_id, intro_date = date, intro_year)

bills_df <- bills_df |>
  left_join(most_recent_action, by = c("id"="bill_id")) |>
  left_join(intro_date, by = c("id"="bill_id")) |>
  rename(state = jurisdiction) |>
  mutate(
    bill_type = str_replace_all(identifier, "[0-9\\s]+$", ""),
    bill_type = str_to_upper(bill_type),
    bill_type = str_replace_all(
      bill_type,
      c("HB"='H','SB'='S','LB'='L','AB'='A',"HS"='H','HF'='H',
        'SF'='S','SS'='S','SD'='S','HD'='H',
        'HF'='H','SF'='S','HP'='H','SP'='S',
        'HRB'='H','SRB'='S','LD'='H')),
    bill_number = str_replace_all(identifier, "^[A-Z\\s]+",""),
    uuid = glue("{state}{intro_year}{bill_type}{bill_number}"),
    session_identifier = as.character(session_identifier),
    state_url = NA) |>
  rename(
    session = session_identifier,
    state_bill_id = identifier,
    description = subject,
    status = last_action_description,
  )

# Create dataframe for bill_metadata
bill_metadata <- bills_df |> select(uuid, state, session, state_bill_id, title, description, status, state_url)

# Create dataframe for sponsors
sponsors <- sponsorships_df |>
  left_join(bills_df, by = c("bill_id" = "id")) |>
  mutate(
    sponsor_type = case_when(
      classification.x == "primary" ~ "sponsor",
      primary == 'True' ~ "sponsor",
      classification.x == "cosponsor" ~ "cosponsor",
      TRUE ~ "sponsor"
    )
  ) |>
  rename(sponsor_name = name) |>
  select(uuid, state, session, state_bill_id, sponsor_name, sponsor_type) |>
  group_by(uuid, state, session, state_bill_id) |>
  nest(sponsors = c(sponsor_name, sponsor_type)) |>
  ungroup()

# Create dataframe for bill_history
bill_history <- bill_actions_df |>
  left_join(bills_df, by = c("bill_id" = "id")) |>
  mutate(
    chamber = case_when(
      organization_id == 'ocd-organization/20782ae5-cc22-4470-846b-fd9788ad3db7' ~ 'Senate',  # Illinois Senate
      organization_id == 'ocd-organization/76120f9a-2982-4cc5-a6e2-de7401fa0d38' ~ 'House',  # Illinois House
      TRUE ~ NA_character_
    ),
    action = glue("{chamber}: {description.x}")
  ) |>
  select(uuid, state, session, state_bill_id, date, action) |>
  group_by(uuid, state, session, state_bill_id) |>
  nest(history = c(date, action)) |>
  ungroup()

# Create dataframe for votes
votes <- votes_df |>
  left_join(bills_df |> select(id, state, state_bill_id, uuid, session), by = c("bill_id" = "id")) |>
  filter(session >= '2001') |>
  mutate(
    description = ifelse(str_detect(motion_classification, "passage"), glue("Passage - {motion_text}"), motion_text) |>
      str_replace_all("\\\\"," "),
    chamber = case_when(
      organization_id == 'ocd-organization/20782ae5-cc22-4470-846b-fd9788ad3db7' ~ 'S',  # Illinois Senate
      organization_id == 'ocd-organization/76120f9a-2982-4cc5-a6e2-de7401fa0d38' ~ 'H',  # Illinois House
      TRUE ~ NA_character_
    ),
    date = ymd(as_date(start_date))
  ) |>
  left_join(
    vote_counts_df |> 
      select(-id) |>
      mutate(
        option = case_match(
          option,
          "yes" ~ "yeas",
          "no" ~ "nays",
          .default = "other"
      )) |>
      pivot_wider(names_from = option, values_from = value,values_fn = sum),
    by = c("id" = "vote_event_id")) |>
  left_join(
    vote_people_df |> select(vote_event_id, response = option, name = voter_name),
    by = c("id" = "vote_event_id")) |>
  select(uuid, state, session, state_bill_id, chamber, date, description, yeas, nays, other, name, response) |>
  group_by(uuid, state, session, state_bill_id, chamber, date, description, yeas, nays, other) |>
  nest(roll_call = c(name, response)) |>
  ungroup()

rm(bill_actions, bill_actions_df, bills, bills_df, intro_date, most_recent_action, sponsorships, sponsorships_df, votes_df, vote_people, vote_people_df, vote_counts, vote_counts_df)

# Function to create output -----------------------------------------------
scrape_bill <- function(UUID){
  OUTPUT_PATH <- 'IL/output'
  # Save bill_metadata
  bill_metadata |> filter(uuid == UUID) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/bill_metadata/{UUID}.json"))
  
  # Save sponsors
  sponsors |> filter(uuid == UUID) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/sponsors/{UUID}.json"))
  
  # Save bill_history
  bill_history |> filter(uuid == UUID) |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/bill_history/{UUID}.json"))
  
  # Save votes
  output_votes <- votes |> filter(uuid == UUID)
  if(nrow(output_votes) == 1){
    vote_date <- output_votes |> pull(date) |> as.character() |> str_replace_all("-","")
    output_votes |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/votes/{UUID}_{vote_date}.json"))
  } else if(nrow(output_votes) > 1) {
    for(i in 1:nrow(output_votes)){
      vote_date = output_votes[i,] |> pull(date) |> as.character() |> str_replace_all("-","")
      output_votes[i,] |> as.list() |> toJSON(auto_unbox = T, pretty = T) |> writeLines(glue("{OUTPUT_PATH}/votes/{UUID}_{vote_date}_{i}.json"))
    }
  }
}

# Collect data ------------------------------------------------------------
bills_to_process <- read_csv("IL/output/il_bills_to_process.csv") |>
  filter(!(session %in% c(91,92)))

future_map(unique(bills_to_process$UUID),
  ~scrape_bill(.x),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE))

