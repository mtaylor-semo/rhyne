
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# Read Data ---------------------------------------------------------------

# Ignore the read warning. It says it expects 23 columns but only
# finds 22, which is the correct number of columns.
ebird_miller_raw <- read_csv(
  "data/MyEBirdData.csv", show_col_types = FALSE) |> 
  janitor::clean_names() %>% 
  filter(location == "Miller Reserve") %>% 
  arrange(date, time, submission_id, taxonomic_order)


# Remove unneeded columns
ebird_miller <- ebird_miller_raw |> 
  select(-state_province, 
         -county, 
         -location_id, 
         -protocol,
         -all_obs_reported, 
         -area_covered_ha,
         -latitude,
         -longitude,
         -ml_catalog_numbers) |> 
  filter(year(date) == 2024)

# create a taxonomy tibble for later sorting
taxonomy <- ebird_miller |> 
  select(common_name, taxonomic_order) |> 
  distinct(common_name, taxonomic_order)

# Calculate total time in field
# 1875 minutes, 31.25 hours
ebird_miller |> 
  distinct(submission_id, .keep_all = TRUE) |> 
  summarize(
    total_min = sum(duration_min),
    total_hrs = total_min/60,
    mean_min = mean(duration_min),
    total_dist = sum(distance_traveled_km),
    mean_dist = mean(distance_traveled_km)) |> 
  write_csv(file = "results/time_in_field.csv")


# Summarize mean number of individuals reported per species.
mean_ind <- ebird_miller |> 
  group_by(common_name) |> 
  summarize(
    max_n = max(count),
    mean_n = mean(count))

num_checks <- ebird_miller |> 
  group_by(common_name) |> 
  count(submission_id, taxonomic_order) |> 
  summarize(
    num_checks = sum(n)
  ) 

obs_data <- num_checks |>
  left_join(mean_ind) |>
  arrange(desc(num_checks), desc(max_n)) |> 
  write_csv(file = "results/species_observations.csv")


# Shannon Diversity Index -------------------------------------------------
# Calculate Shannon diversity based on maximum observed number of
# individuals observed for any one checklist.
# Not sure it's worth using.
sw_div <- obs_data |> 
  mutate(
    pi = max_n/sum(max_n),
    pi_ln_pi = pi * log(pi))

(h_prime = -sum(sw_div$pi_ln_pi))

by_visit_div <- ebird_miller |> 
  group_by(submission_id, date) |> 
  mutate(
    pi = count/sum(count),
    pi_ln_pi = pi * log(pi)
  ) |> 
  summarize(by_visit = -sum(pi_ln_pi), .groups = "keep")

(mean(by_visit_div$by_visit))


# Number of species per month ---------------------------------------------
# Count number of species observed per month
ebird_miller |> 
  group_by(month(date)) |>
  distinct(common_name) |> 
  count(common_name) |> 
  summarize(n_obs = sum(n)) |> 
  write_csv(file = "results/species_per_month.csv")

ebird_miller |> 
  group_by(month(date)) |>
  distinct(date) |> 
  count(submission_id) |> 
  summarize(n_obs = sum(n)) |> 
  write_csv(file = "results/visits_per_month.csv")

ebird_miller |> 
  group_by(date) |> 
  distinct(date) |> 
  group_by(month(date)) |> 
  count()

ebird_miller |> 
  group_by(month(date)) |> 
  distinct(submission_id, .keep_all = TRUE) |> 
  summarise(
    total_time = sum(duration_min),
    total_dist = sum(distance_traveled_km)
  )


