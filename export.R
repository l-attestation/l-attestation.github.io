# Packages ----

library(tidyverse)
library(targets)
library(questionr)
library(countrycode)
library(sf)

# Paths ----

tar_load(ends_with("_file"))

# World sf ----
# ok : mind Somaliland & Western Sahara
# PSE = Palestine
# north macedonia : ok
# montenegro : ok

world_sf = rnaturalearth::ne_countries(type = "countries", returnclass = "sf") |>
  select(admin, continent) |> 
  mutate(wb = countrycode(admin, origin = "country.name", destination = "wb")) |> 
  filter(!is.na(wb)) |> 
  filter(admin != "Northern Cyprus")
length(unique(world_sf$wb)) == length(unique(world_sf$admin))
filter(world_sf, n() > 1, .by = wb)

# World Bank ----
# Territoires palestiniens = PSE = West Bank (Cisjordanie) & Gaza
wb = read_rds(wb_file)
names_wb = count(wb, country, iso3c) |> 
  mutate(wb = countrycode(country, origin = "country.name", destination = "wb"))
freq.na(names_wb)
filter(names_wb, n() > 1, .by = wb)

# Oxcgrt ----
# Palestine ok : PSE
# montenegro et macédoine du Nord absents

oxcgrt = read_rds(oxcgrt_file)
names_oxcgrt = count(oxcgrt, CountryName) |> 
  mutate(wb = countrycode(CountryName, origin = "country.name", destination = "wb"))
freq.na(names_oxcgrt)
filter(names_oxcgrt, n() > 1, .by = wb)
filter(oxcgrt, n() > 1, .by = c(CountryName, Date))
# Fariss ----
# pas la palestine
fariss = read_csv(fariss_file) |> 
  filter(YEAR == 2019)
names_fariss = count(fariss, country_name) |> 
  mutate(wb = countrycode(country_name, origin = "country.name", destination = "wb"))
freq.na(names_fariss)
filter(names_fariss, n() > 1, .by = wb)
# Google ----
# la réunion présentée séparemment
# north macedonia ok
# pas palestine ni montenegro

google = arrow::open_dataset(google_file) |> 
  filter(is.na(sub_region_1),
         is.na(sub_region_2)) |>
  collect()

names_google = count(google, country_region, country_region_code) |>
  mutate(wb = countrycode(country_region, origin = "country.name", destination = "wb"))
filter(names_google, n() > 1, .by = wb)
freq.na(names_google)
google_duplicates = filter(google, n() > 1, .by = c(country_region, date))

# Excess mortality ----
# pbs départements d'outre mer, RU & Transnitrie (Moldavie occupée Russie)
excess_mortality = read_rds(excess_mortality_file)
names_excess_mortality = excess_mortality |> 
  mutate(location = if_else(location %in% c("Scotland",
                                            "England & Wales",
                                            "Northern Ireland"),
                            "United Kingdom",
                            location),
         location = if_else(location %in% c("French Guiana",
                                            "Guadeloupe",
                                            "Mayotte",
                                            "Martinique",
                                            "Reunion"),
                            "France",
                            location)) |> 
  count(location) |> 
  mutate(wb = countrycode(location, origin = "country.name", destination = "wb"))
filter(names_excess_mortality, n() > 1, .by = wb)

# Apple ----

apple = read_rds(apple_file)
names_apple = count(apple, iso3c) |> 
  mutate(wb = countrycode(iso3c, origin = "iso3c", destination = "wb"))
filter(names_apple, n() > 1, .by = wb)

# AGR ----

agr = read_rds(agr_file)
names_agr = count(agr, country) |> 
  mutate(wb = countrycode(country, origin = "country.name", destination = "wb")) |> 
  filter(country != "Northern Cyprus")
filter(names_agr, n() > 1, .by = wb)

# Police ----
# GB là encore séparé
police_europe = read_rds(police_europe_file)
names_police_europe = count(police_europe, geo) |> 
  mutate(wb = countrycode(geo, origin = "country.name", destination = "wb"))
filter(names_police_europe, n() > 1, .by = wb)

# Données à envoyer ----

tar_load(world_country_date)
tar_load(wb_police_europe_pcmh)
tar_load(excess_mortality)
tar_load(world_summarise)
tar_load(world_composite_score)
x = filter(world_summarise, n() > 1, .by = wb)
freq.na(world_composite_score)
arrange(world_composite_score, desc(indice_enfermement))

world = world_summarise |> 
  left_join(world_composite_score, by = join_by(pays))
write_csv(world, "attestation-world.csv")
read_csv("attestation-world.csv")
# Count duplicates ----

duplicates = world_country_date |>
  filter(n() > 1, .by = c(wb, date)) |> 
  arrange(wb, date)
freq.na(duplicates)
count(duplicates, n)
count(duplicates, wb)

# Final error ----

tar_load(excess_mortality)

errors = excess_mortality |> 
  filter(wb %in% c("FRA", "GBR")) |> 
  select(-p_scores_all_ages)
  summarise(across(ends_with("_all_ages"), sum), .by = c(wb, date))
