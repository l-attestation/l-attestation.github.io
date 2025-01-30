# Packages ----

library(tidyverse)
library(targets)

# Targets ----

tar_load(excess_mortality_file)

excess_mortality = read_rds(excess_mortality_file) |>
  filter(location == "France")
  mutate(location2 = if_else(location %in% c("Scotland",
                                            "England & Wales",
                                            "Northern Ireland"),
                            "United Kingdom",
                            location),
         location2 = if_else(location %in% c("French Guiana",
                                            "Guadeloupe",
                                            "Mayotte",
                                            "Martinique",
                                            "Reunion"),
                            "France",
                            location2)) |> 
  select(location2, location, date,
         excess_proj_all_ages,
         projected_deaths_since_2020_all_ages) |> 
  filter(location2 %in% c("United Kingdom", "France"),
         between(date, ymd(20200301), ymd(20200601))) |> 
  arrange(location2, date)

freq.na(excess_mortality)
