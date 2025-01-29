# Packages-----

library(targets)
library(tarchetypes)

# Options-----

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse", "readxl",
                            "lubridate", "ggrepel",
                            "sf", "countrycode"))
# Functions ----

source("functions.R")

tar_plan(
  
  # Files-------
  
  tar_file(agr_file, here::here("data", "agr.rds")),
  tar_file(excess_mortality_file, here::here("data", "excess_mortality.rds")),
  tar_file(google_file, here::here("data", "google", "part-0.parquet")),
  tar_file(oxcgrt_file, here::here("data", "oxcgrt.rds")),
  tar_file(wb_file, here::here("data", "wb.rds")),
  tar_file(police_europe_file, here::here("data", "crim_just_job.rds")),
  tar_file(exceptius_file, here::here("data", "indicateurs exceptius paper01.xlsx")),
  tar_file(apple_file, here::here("data", "apple.rds")),
  tar_file(fariss_file, here::here("data", "HumanRightsProtectionScores_v4.01.csv")),
  
  tar_file(antai_file, here::here("data", "verbalisations_departement.xlsx")),
  tar_file(controles_file, here::here("data", "controles_medias.ods")),
  tar_file(population_file, here::here("data", "population_departement.xls")),
  tar_file(departements_sf_file, "~/geo/frdep2020.gpkg"),
  tar_file(pm_file, here::here("data", "polices_municipales.ods")),
  tar_file(communes_sf_file, "~/geo/frcom2020.gpkg"),
  tar_file(vigilants_file, here::here("data", "2020-05-09_vigilants.rds")),
  
  
  # Font Family-----
  
  geom_fontfamily = "Serif",
  
  # Dates--------
  
  date_beginn = ymd(20200301),
  date_end = ymd(20200601),
  
  # Natural Earth ----------
  
  world_sf = rnaturalearth::ne_countries(type = "countries", returnclass = "sf") |>
    select(admin, continent) |>
    mutate(wb = countrycode(admin, origin = "country.name", destination = "wb")) |>
    select(-admin),
  
  # World Bank----------
  
  wb = read_rds(wb_file) |>
    mutate(country = str_replace(country, "Turkiye", "Turkey"),
           wb = countrycode(country, origin = "country.name", destination = "wb")) |>
    rename(year = date) |>
    select(-iso2c, -iso3c, -country),
  
  # OxCGRT Stringency index--------
  
  oxcgrt = read_rds(oxcgrt_file) |>
    janitor::clean_names() |>
    mutate(date = ymd(date),
           wb = countrycode(country_name, origin = "country.name", destination = "wb"),
           national_or_subnational_saho = case_when(c6m_stay_at_home_requirements %in% 0:1 ~ 0,
                                                    c6m_stay_at_home_requirements == 2 ~ 1,
                                                    c6m_stay_at_home_requirements == 3 ~ 2),
           national_saho = national_or_subnational_saho *  c6m_flag) |>
    select(wb,
           date,
           stringency_index_average,
           national_or_subnational_saho,
           national_saho),
  
  # Exceptius---------
  
  exceptius_decreter = read_excel(exceptius_file, range = "A136:B159") |>
    janitor::clean_names() |>
    rename(pays = indice_de_severite_des_mesures,
           score_decrets = x2),
  
  exceptius_controler = read_excel(exceptius_file, range = "C136:D159") |>
    janitor::clean_names() |>
    rename(pays = score_de_controle_policier,
           score_controle = x2) |>
    mutate(pays = if_else(pays == "Irelande", "Irlande", pays)),
  
  exceptius = exceptius_decreter |>
    left_join(exceptius_controler, by = "pays") |>
    mutate(pays = str_replace(pays, "Pays-bas", "Pays-Bas"),
           pays = str_replace(pays, "Royaume Uni", "Royaume-Uni")) |>
    mutate(wb = countrycode(pays, origin = "country.name.fr", destination = "wb")) |>
    select(-pays),
  
  # Fariss---------
  
  fariss = read_csv(fariss_file) |>
    janitor::clean_names() |>
    filter(year == 2019) |>
    rename(hr_fariss = theta_mean)  |> 
    mutate(wb = countrycode(country_name, origin = "country.name", destination = "wb")) |> 
    select(wb, hr_fariss),
  
  # Google ------
  
  google = arrow::open_dataset(google_file) |>
    filter(is.na(sub_region_1),
           is.na(sub_region_2),
           is.na(metro_area),
           country_region != "Réunion") |>
    collect() |> 
    mutate(date = ymd(date),
           wb = countrycode(country_region, origin = "country.name", destination = "wb")) |>
    select(wb, date, ends_with("_baseline")),
  
  # Excess mortality------
  
  excess_mortality = read_rds(excess_mortality_file) |>
    filter(!(location %in% c("Scotland",
                             "England & Wales",
                             "Northern Ireland",
                             "French Guiana",
                             "Guadeloupe",
                             "Mayotte",
                             "Martinique",
                             "Reunion",
                             "Transnistria"))) |> 
    mutate(wb = countrycode(location, origin = "country.name.en", destination = "wb")) |>
    select(wb,
           date,
           deaths_2020_all_ages,
           projected_deaths_since_2020_all_ages,
           excess_proj_all_ages),
  
  # Police Europe--------
  
  police_europe = read_rds(police_europe_file)  |> 
    pivot_wider(names_from = c(sex, isco08, unit),
                values_from = "values",
                names_repair = janitor::make_clean_names) |> 
    complete(geo, time) |> 
    mutate(geo = if_else(geo %in% c("Scotland", "England and Wales", "Northern Ireland (UK)"),
                         "United Kingdom", geo)) |>
    summarise(across(ends_with("number"), ~ sum(.x, na.rm = F)), .by = c(geo, time)) |> 
    mutate(geo = str_replace(geo, "Türkiye", "Turkey"),
           geo = if_else(str_detect(geo, "Germany"), "Germany", geo),
           geo = if_else(str_detect(geo, "Kosovo"), "Republic of Kosovo", geo),
           year = year(time),
           wb = countrycode(geo, origin = "country.name", destination = "wb")) |>
    select(wb, year, total_police_officers_number),
  
  wb_police_europe_pcmh = police_europe |>
    filter(!is.na(total_police_officers_number),
           year <= 2020) |> 
    filter(year == max(year, na.rm = TRUE), .by = wb) |> 
    left_join(wb, by = join_by(wb, year)) |>
    mutate(policiers_pcm = total_police_officers_number / population * 100000),
  
  # AGR-------
  
  agr = read_rds(agr_file) |>
    filter(country != "Northern Cyprus") |>
    mutate(date = ymd(date),
           wb = countrycode(country, origin = "country.name", destination = "wb")) |>
    select(-country),
  
  agr_mode = agr |>
    filter(between(date, ymd(20200401), ymd(20200501))) |>
    mutate(least_strict_day = str_remove_all(least_strict_day, "b"),
           least_strict_day = as.numeric(least_strict_day),
           least_strict_day = case_when(
             least_strict_day %in% 0:1 ~ "Sorties totalement libres",
             least_strict_day %in% 2:3 ~ "Sorties à justifier",
             least_strict_day %in% 4:5 ~ "Excercice physique limité",
             least_strict_day %in% 6:7 ~ "Excercice physique interdit")) |>
    count(wb, least_strict_day) |>
    group_by(wb) |>
    filter(n == max(n)) |>
    rename(mode_least_strict_day_april = least_strict_day) |>
    select(-n),
  
  # Apple-------
  
  apple = read_rds(apple_file) |>
    mutate(date = ymd(date),
           wb = countrycode(iso3c, origin = "iso3c", destination = "wb")) |>
    select(-iso3c),
  
  # Join--------
  
  ## Country-Date-----------
  
  world_country_date = oxcgrt |>
    full_join(agr, by = join_by(wb, date)) |>
    full_join(google, by = join_by(wb, date)) |>
    full_join(apple, by = join_by(wb, date)) |>
    full_join(excess_mortality, by = join_by(wb, date)) |>
    filter(between(date, date_beginn, date_end)) |> 
    # distinct(wb, date, .keep_all = TRUE) |>
    relocate(wb, date),
  
  ## Summarise---------
  
  world_summarise = world_sf |>
    st_set_geometry(NULL) |>
    left_join(world_country_date) |>
    group_by(wb, continent) |>
    summarise(stringency_index_average = mean(stringency_index_average, na.rm = T), # oxcgrt
              across(ends_with("saho"), ~ sum(.x, na.rm = T)),
              across(starts_with("stayhome"), ~ sum(.x == "yes")), # agr
              across(starts_with("walk"), ~ sum(.x == "no")),
              across(ends_with("_baseline"), ~ mean(.x, na.rm = T)), # google
              across(c("driving", "walking", "transit"), ~ mean(.x, na.rm = T)), # apple
              p_score = sum(excess_proj_all_ages, na.rm = T) / sum(projected_deaths_since_2020_all_ages, na.rm = T) * 100,
              .groups = "drop") |>
    left_join(wb_police_europe_pcmh) |>
    left_join(agr_mode) |>
    left_join(exceptius) |>
    left_join(fariss) |>
    mutate(pays = countrycode(wb, origin = "wb", destination = "country.name.fr"),
           pays = if_else(wb == "MNE", "Monténégro", pays),
           pays = if_else(wb == "MKD", "Macédoine du Nord", pays)) |>
    distinct(wb, .keep_all = T) |>
    relocate(pays, wb, continent),
  
  ## Composite score--------
  
  world_composite_score = world_summarise |>
    select(pays,
           residential_percent_change_from_baseline,
           parks_percent_change_from_baseline,
           walking,
           driving,
           national_saho,
           national_or_subnational_saho) |>
    mutate(parks_percent_change_from_baseline = - parks_percent_change_from_baseline,
           walking = 100 - walking,
           driving = 100 - driving,
           across(where(is.numeric), scale)) |>
    pivot_longer(cols = 2:7) |>
    group_by(pays) |>
    summarise(indice_enfermement = mean(value, na.rm = T)),
  
  # Plots---------
  
  ## Stringency Index-------
  
  stringency_index_polar_plot = world_summarise |>
    
    filter(continent == "Europe",
           !is.na(stringency_index_average)) |>
    
    mutate(pays = str_wrap(pays, 5, whitespace_only = F),
           pays = fct_reorder(pays, stringency_index_average)) |>
    
    ggplot2::ggplot(aes(y = stringency_index_average, x = pays)) +
    
    geom_hline(
      aes(yintercept = y),
      data.frame(y = c(0:9) * 10),
      color = "lightgrey"
    ) +
    
    geom_bar(stat = "identity",
             # colour = "red"
    ) +
    
    scale_y_continuous(
      limits = c(-10, 110),
      expand = c(0, 0),
      breaks = NULL) +
    
    coord_polar(start = 0) +
    
    labs(x = "",
         y = "",
         title = "Ampleur des restrictions dans les États européens,\nentre le 1er mars et le 1er juin 2020",
         # subtitle = "Maximale en Italie, au Kosovo et en France,\nminimale en Biélorussie, en Islande et en Lettonie",
         # subtitle = str_glue("Entre le {format(date_beginn, '%d %B %Y')} et le {format(date_end, '%d %B %Y')}"),
         caption = "Données : Stringency Index, Oxford Covid-19 Government Response Tracker (OxCGRT)") +
    
    theme(axis.text = element_text(size = 7),
          legend.position = "none"),
  
  stringency_index_plot = world_summarise |>
    filter(continent == "Europe",
           pays != "Biélorussie",
           !is.na(stringency_index_average)) |>
    mutate(
      # pays = str_wrap(pays, 5, whitespace_only = F),
      pays = fct_reorder(pays, stringency_index_average)) |>
    ggplot(aes(x = stringency_index_average, y = pays)) +
    geom_point() +
    coord_cartesian(xlim = c(40, NA)) +
    labs(x = "Stringency Index moyen (01/03 au 01/06)",
         y = "",
         caption = "Données : Stringency Index, Oxford Covid-19 Government Response Tracker (OxCGRT)"),
  
  ## Exceptius----------
  
  exceptius_plot = world_summarise |>
    ggplot2::ggplot(aes(x = score_decrets, y = score_controle)) +
    geom_point(alpha = 0.6) +
    geom_text_repel(aes(label = pays),
                    size = 3, 
                    family = geom_fontfamily,
                    min.segment.length = Inf,
                    # color = "white",     # text color
                    # bg.color = "grey30", # shadow color
                    # bg.r = 0.15          # shadow radius
                    # direction = "y",
                    # hjust = "left",
                    # point.padding = 0.2, 
                    # nudge_x = .15,
                    # nudge_y = .5,
                    # segment.curvature = -1e-20,
                    # arrow = arrow(length = unit(0.015, "npc"))
    ) +
    
    labs(title = "Une minorité d'États européens ont mis en place des mesures de surveillance draconniennes",
         subtitle = "Tous se situent à l'Est et au Sud de l'Europe. La France voisine avec la Pologne et la Roumanie",
         x = "Score de limitation des libertés",
         y = "Score de surveillance",
         caption = "Données : Exceptius"),
  
  ## Parks average------------
  
  parks_average_plot = world_summarise |>
    
    filter(continent == "Europe",
           !is.na(parks_percent_change_from_baseline)) |>
    
    mutate(pays = fct_reorder(pays, desc(parks_percent_change_from_baseline))) |>
    
    ggplot2::ggplot(aes(y = pays, x = parks_percent_change_from_baseline)) +
    
    geom_bar(stat="identity",
             alpha = 1) +
    
    labs(x = "Fréquentation des espaces verts du 1er mars au 1er juin par rapport à janvier 2020 (%)",
         y = "",
         title = "Européens à l'air libre au Nord, enfermés au Sud et à l'Est",
         subtitle = "La fréquentation des espaces verts *baisse* de 50% en Italie et *augmente* de 80% au Danemark",
         caption = "Données : Google Mobility Reports"),
  
  ## Parks evolution-------
  
  selection_pays_europe = c("France", "Espagne", "Italie", "Pays-Bas", "Allemagne", "Danemark"),
  
  parks_evolution_plot = world_country_date |>
    
    mutate(pays = countrycode(wb, origin = "wb", destination = "country.name.fr"),
           pays = fct_relevel(pays, selection_pays_europe),
           parks_roll7 = zoo::rollmean(parks_percent_change_from_baseline, k = 7, fill = NA)) |>
    
    filter(pays %in% selection_pays_europe) |>
    
    ggplot2::ggplot(aes(x = date, y = parks_roll7)) +
    
    geom_line() +
    
    facet_wrap(~ pays) +
    
    labs(title = "La signature des confinements latins",
         subtitle = "Un effondrement brutal et prolongé des sorties d'aération",
         x = "",
         y = "Fréquentation des espaces verts par rapport à janvier\n(Moyenne glissante sur 7 jours)",
         caption = "Données : Google Mobility Reports"),
  
  ## Police Europe---------
  
  police_europe_plot = world_summarise |>
    
    filter(continent == "Europe",
           !is.na(policiers_pcm)) |>
    
    mutate(pays = fct_reorder(pays, policiers_pcm)) |>
    
    ggplot2::ggplot(aes(y = pays, x = policiers_pcm)) +
    
    geom_bar(stat = "identity"),
  
  ## Fariss & Parks--------
  
  fariss_parks_plot = world_summarise |>
    
    filter(!is.na(policiers_pcm),
           !pays %in% c("Turquie", "Luxembourg")) |>
    
    ggplot(aes(x = hr_fariss, y = parks_percent_change_from_baseline)) +
    
    geom_point() +
    
    geom_text_repel(aes(label = pays), family = geom_fontfamily, size = 3) +
    
    # scale_x_continuous(limits = c(1, 4)) +
    
    labs(
      # title = "Plus les Étas européens respectaient le droit à l'intégrité physique,\nmoins ils ont enfermé leur population",
      x = "Droit à l'intégrité physique 2019 (Indice synthétique)",
      y = "Fréquentation des espaces verts du 1er mars au 1er juin\npar rapport à janvier 2020 (%)",
      caption = "Données : Latent Human Rights Protection Scores Version 4 & Google Mobility Reports"),
  
  ## Police & Parks---------
  
  police_parks_plot = world_summarise |>
    ggplot(aes(x = policiers_pcm, y = parks_percent_change_from_baseline)) +
    geom_point() +
    geom_text_repel(aes(label = pays), family = geom_fontfamily, size = 3) +
    scale_x_continuous(limits = c(130, 510)) +
    labs(title = "Plus les États européens comptaient de policiers par habitant,\nplus ils ont enfermé leur population",
         x = "Policiers et assimilés pour 100 000 habitants",
         y = "Fréquentation des espaces verts du 1er mars au 1er juin\npar rapport à janvier 2020 (%)",
         caption = "Données : Eurostat & Google Mobility Reports"),
  
  ## AGR mode ----
  
  agr_mode_plot = world_summarise |>
    filter(!is.na(mode_least_strict_day_april)) |>
    mutate(mode_least_strict_day_april = fct_reorder(mode_least_strict_day_april,
                                                     policiers_pcm)) |> 
    select(pays, mode_least_strict_day_april) |> 
    mutate(x = 1) |> 
    ggplot() +
    ggrepel::geom_text_repel(aes(x = x, y = x, label = pays),
                             position = position_jitter(seed = 1),
                             family = geom_fontfamily, size = 3) +
    facet_wrap(~mode_least_strict_day_april) +
    theme_minimal() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(linewidth = 2),
    ),
  
  ## AGR & Police-----------
  
  agr_police_boxplot = world_summarise |>
    filter(!is.na(mode_least_strict_day_april),
           !is.na(policiers_pcm)) |>
    mutate(mode_least_strict_day_april = str_wrap(mode_least_strict_day_april, 5),
           mode_least_strict_day_april = fct_reorder(mode_least_strict_day_april,
                                                     policiers_pcm)) |>
    ggplot(aes(x = mode_least_strict_day_april,
               y = policiers_pcm)) +
    geom_boxplot(varwidth = TRUE) +
    geom_label_repel(aes(label = pays), family = geom_fontfamily, size = 2.5) +
    labs(title = "La concentration policière augmente avec le niveau des restrictions",
         subtitle = "La France est dans le petit groupe des pays à attestation",
         y = "Policiers et assimilés pour 100 000 habitants",
         x = "",
         caption = "Données : A Good Reason & Eurostat") +
    theme(axis.text.x = element_text(face= "bold")),
  
  agr_police_plot = world_summarise |>
    filter(!is.na(mode_least_strict_day_april),
           !is.na(policiers_pcm)) |>
    mutate(mode_least_strict_day_april = str_wrap(mode_least_strict_day_april, 5),
           mode_least_strict_day_april = fct_reorder(mode_least_strict_day_april,
                                                     policiers_pcm)) |>
    ggplot(aes(x = mode_least_strict_day_april,
               y = policiers_pcm)) +
    geom_label_repel(aes(label = pays), family = geom_fontfamily, size = 2.5) +
    stat_summary(fun = "mean", geom = "point", size = 2, color = "red") +
    labs(
      # title = "La concentration policière augmente avec le niveau des restrictions",
      # subtitle = "La France est dans le petit groupe des pays à attestation",
      y = "Policiers et assimilés pour 100 000 habitants",
      x = "",
      caption = "Données : A Good Reason & Eurostat") +
    theme(axis.text.x = element_text(face= "bold")),
  
  ## Indice and p_score--------
  
  indice_p_score_plot = world_summarise |>
    left_join(world_composite_score) |>
    ggplot2::ggplot(aes(x = indice_enfermement, y = p_score)) +
    geom_point() +
    geom_text_repel(aes(label = pays), family = geom_fontfamily, size = 3) +
    labs(title = "Où passer le prochain ?",
         subtitle = "Dans les pays qui n'ont connu ni restriction de la liberté d'aller et venir,\nni augmentation de la mortalité",
         x = "Indice synthétique d'enfermement",
         y = "Excès de mortalité en % de la mortalité attendue (p-score)",
         caption = "Données : Oxford Covid19 Government Response Tracker, Google Mobility Reports, Apple Mobility Reports\nHuman Mortality Database, World Mortality Dataset, World Health Organization (via Our World in Data)"),
  
  # Verbalisations----
  
  verbalisations_departement = read_excel(antai_file, skip = 2) |> 
    rename(code_departement = "Département d'infraction",
           verbalisations = "Nombre de dossiers d'infraction"),
  
  # Contrôles ----
  
  controles = rio::import(controles_file) |> 
    filter(zone == "Toutes",
           jours > 20,
           !is.na(controles)) |> 
    group_by(nom_departement) |> 
    slice_max(jours) |> 
    ungroup() |> 
    mutate(controles = controles / jours * 54) |>
    select(nom_departement, controles),
  
  # Plus de 15 ans----
  
  population_departement = read_excel(population_file,
                                      sheet = "2020",
                                      range = "A5:W109") |>
    rename(code_departement = "...1",
           nom_departement = "...2",
           population_totale = Total) |> 
    rowwise() |> 
    mutate(population_plus_de_15_ans = sum(c_across("15 à 19 ans":"95 ans et plus"))) |>
    ungroup() |> 
    select(code_departement, nom_departement, population_plus_de_15_ans, population_totale) |> 
    filter(!str_detect(code_departement, "France"),
           !str_detect(code_departement, "DOM")),
  
  # Residential----
  
  # Dep sf----
  
  outre_mer = c("Guyane", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"),
  
  # Départements-----
  
  chop_ci = function(x, style, n = 4) {
    santoku::chop(x,
                  classInt::classIntervals(x, n = n, style = style)$brks,
                  labels = santoku::lbl_dash("-", fmt = round))
  },
  
  departements = population_departement |>
    left_join(verbalisations_departement, by = "code_departement") |> 
    left_join(controles, by = "nom_departement") |> 
    mutate(verbalisations_pmla = verbalisations / population_plus_de_15_ans * 1000,
           controles_pmla = controles / population_plus_de_15_ans * 1000,
           v_headtails = chop_ci(verbalisations_pmla, "headtails"),
           v_fisher = chop_ci(verbalisations_pmla, "fisher")),
  
  # Municipaux-----
  
  pm = readODS::read_ods(pm_file,
                         skip = 8,
                         col_names = FALSE,
                         .name_repair = janitor::make_clean_names) |> 
    select(x, x_2, x_4:x_10) |> 
    rename(code_departement = x,
           nom_departement = x_2,
           nom_commune = x_4,
           population = x_5,
           municipaux = x_6,
           asvp = x_7,
           gardes_champetres = x_8,
           maitres_chiens = x_9,
           chiens_patrouille = x_10) |> 
    mutate(total = municipaux + gardes_champetres + asvp) |> 
    filter(!is.na(code_departement)),
  
  pm_com = pm |> 
    filter(!str_detect(code_departement, "TOTAL")) |> 
    mutate(code_departement = if_else(code_departement %in% as.character(1:9),
                                      str_c("0", code_departement),
                                      code_departement),
           nom_commune_clean = stringi::stri_trans_general(nom_commune, "Latin-ASCII"),
           nom_commune_clean = fedmatch::clean_strings(nom_commune_clean),
           population = as.numeric(population)) |>
    select(code_departement, nom_commune_clean, total),
  
  pm_dep = pm |> 
    filter(str_detect(code_departement, "TOTAL"),
           !str_detect(code_departement, "NATIONAL")) |> 
    mutate(total = municipaux + gardes_champetres + asvp,
           departement = str_remove(code_departement, "^TOTAL ")) |>
    select(departement, total) |> 
    arrange(desc(total)),
  
  communes_securitaires_map = st_read(communes_sf_file, quiet = T) |> 
    inner_join(pm_com, by = join_by(code_departement, nom_commune_clean)) |> 
    mutate(policiers_pml = total / population_2017 * 1000,
           p2 = santoku::chop_quantiles(policiers_pml, c(0.7))) |>
    ggplot() +
    geom_sf(aes(fill = p2), color = NA) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = c("black", "red")) +
    labs(title = "Densité de policiers municipaux",
         caption = "Source : Enquête 2020 sur les polices municipales") +
    guides(fill = "none"),
  
  # Vigilants----
  
  vigilants = read_rds(vigilants_file) |> 
    mutate(nom_departement = if_else(str_detect(A, "[A-Z]{3,}"), A, NA_character_),
           nom_departement_clean = stringi::stri_trans_general(nom_departement, "Latin-ASCII"),
           nom_departement_clean = fedmatch::clean_strings(nom_departement_clean),
           nom_commune = str_remove(A, "\\(.+\\)"),
           nom_commune = str_squish(nom_commune),
           nom_commune_clean = stringi::stri_trans_general(nom_commune, "Latin-ASCII"),
           nom_commune_clean = fedmatch::clean_strings(nom_commune_clean),
           nb_communautes = str_extract(A, "\\(.+\\)"),
           nb_communautes = parse_number(nb_communautes)) |> 
    fill(nom_departement, .direction = "down") |> 
    drop_na(nb_communautes) |> 
    select(-A),
  
  vigilants_plm = vigilants |> 
    mutate(nom_commune_clean = if_else(str_detect(nom_commune_clean, "arrondissement"),
                                       str_extract(nom_commune_clean, "paris|lyon|marseille"),
                                       nom_commune_clean)) |> 
    group_by(nom_departement_clean, nom_commune_clean) |> 
    summarise(nb_communautes = sum(nb_communautes)) |> 
    ungroup(),
  
  vigilants_departements = vigilants |> 
    group_by(nom_departement_clean) |> 
    summarise(nb_communautes = sum(nb_communautes)) |> 
    select(nom_departement_clean, nb_communautes),
  
  vigilants_map = st_read(communes_sf_file, quiet = T) |>  
    inner_join(vigilants_plm, by = join_by(nom_commune_clean)) |> 
    mutate(vigilants_pml = nb_communautes / population_2017 * 1000,
           p2 = santoku::chop_quantiles(vigilants_pml, c(0.7))) |>
    ggplot() +
    geom_sf(aes(fill = p2), color = NA) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = c("black", "red")) +
    labs(title = "Densité de voisins vigilants") +
    guides(fill = "none"),
  
  # Prévalence verbalisation----
  
  prevalence_verbalisation_map = st_read(departements_sf_file, quiet = T) |> 
    left_join(departements, by = c("code_departement", "nom_departement")) |> 
    ggplot() +
    geom_sf(aes(fill = v_headtails), color = NA) +
    coord_sf(datum = NA) +
    viridis::scale_fill_viridis(discrete = T) +
    labs(
      title = "Prévalence de la verbalisation",
      fill = "Pour 1000 adultes",
      caption = "Sources : ANTAI, INSEE"),
  
  # Prevalence shadow ----
  
  prevalence_classint = classInt::classIntervals(departements$verbalisations_pmla,
                                                 style = "headtails"),
  prevalence_verbalisation_shadow = st_read(departements_sf_file, quiet = T) |> 
    left_join(departements,
              by = c("code_departement", "nom_departement")) |>
    ggplot(aes(fill = verbalisations_pmla)) +
    ggfx::with_shadow(geom_sf(color = NA)) +
    scale_fill_viridis_b(breaks = prevalence_classint$brks,
                         labels = round) +
    labs(fill = "Pour 1000 adultes",
         caption = "Source : ANTAI"),
  
  # Taux verbalisation----
  
  taux_verbalisation_plot = departements |>
    ggplot(aes(x = controles_pmla,
               y = verbalisations_pmla)) +
    geom_point() +
    geom_text_repel(aes(label = nom_departement),
                    size = 3,
                    family = geom_fontfamily) +
    labs(title = "Quadriller et punir",
         subtitle = str_wrap("Le *taux de verbalisation* de chaque département correspond à la pente de la droite qui le relie à l'origine", 90),
         x = "Contrôles pour 1000 adultes",
         y = "Verbalisations pour 1000 adultes",
         caption = "Données : Presse (pour les contrôles), ANTAI (pour les verbalisations)"),
  
  # Lignes ----
  
  ## Variables-------
  
  ### Vécu ---------
  
  v_emotions = list(senti_1 = "Fatigué",
                    senti_2 = "Irrité",
                    senti_3 = "Détendu",
                    senti_4 = "En forme",
                    senti_5 = "Stressé",
                    senti_6 = "Triste",
                    senti_7 = "Heureux",
                    senti_8 = "Inquiet"),
  
  v_travail = list(trav_1 = "Licenciement",
                   trav_2 = "Mise en congé",
                   trav_3 = "Chômage technique partiel ou total",
                   trav_4 = "Baisse du temps de travail",
                   trav_5 = "Reprise du travail",
                   trav_6 = "Hausse du temps de travail obligatoire",
                   trav_7 = "Horaires inhabituels",
                   trav_8 = "Dégradation conditions de travail",
                   trav_9 = "Diminution du revenu"),
  
  v_remarques = list(remarq_1 = "Remarques agréables",
                     remarq_2 = "Remarques hostiles"),
  
  v_nuisances = list(logprob_1 = "Pollution",
                     logprob_2 = "Insécurité",
                     logprob_3 = "Saleté",
                     logprob_4 = "Pénurie",
                     logprob_5 = "Bruits extérieurs",
                     logprob_6 = "Conflits de voisinage",
                     logprob_7 = "Bruits domestiques",
                     logprob_8 = "Conflits domestiques",
                     logprob_9 = "Exiguïté"),
  
  v_habitat_tc = list(logtaille = "Taille du logement",
                      logquartier = "Type de quartier", # tc : to coalesce
                      logtype = "Type de logement",
                      logext_1 = "Balcon",
                      logext_2 = "Jardin",
                      logext_3 = "Cour ou terrasse",
                      logext_4 = "Cour d'immeuble"),
  
  v_habitat = c(v_habitat_tc,
                v_nuisances,
                lognb = "Nombre de cohabitants"),
  
  v_police = list(control = "Contrôle",
                  controlamende = "Verbalisation"),
  
  v_vecu = c(v_emotions,
             v_travail,
             v_remarques,
             v_nuisances,
             v_habitat,
             v_police,
             tele = "Lieu de travail",
             ecole = "Aide aux devoirs",
             temps = "Manquer de temps libre",
             courses = "Fréquence des courses",
             activ = "Temps consacré au sport",
             lognb = "Nombre de cohabitants"),
  
  ### Caractéristiques--------
  
  v_demo = list(sexe = "Sexe",
                diploniv_rec = "Diplôme",
                pcs_rec9 = "PCS",
                sitconj = "Situation conjugale",
                natioparents = "Nationalité des parents",
                rev = "Revenu",
                pol = "Préférences partisanes"),
  
  v_age = list(age = "Âge",
               anais = "Année de naissance"),
  
  v_politisation = list(polpart_1 = "Manifesté",
                        polpart_2 = "Fait grève",
                        polpart_3 = "Pétitionné",
                        polpart_4 = "Assisté à un débat",
                        polpart_5 = "Milité"),
  
  v_religion = list(relig = "Religion",
                    religimport = "Importance de la religion",
                    relprat = "Pratique religieuse"),
  
  v_nuisances_avant = list(resprob_1 = "Pollution",
                           resprob_2 = "Insécurité",
                           resprob_3 = "Saleté",
                           resprob_4 = "Bruits extérieurs",
                           resprob_5 = "Conflits de voisinage",
                           resprob_6 = "Bruits domestiques",
                           resprob_7 = "Conflits avec cohabitants",
                           resprob_8 = "Exiguïté"),
  
  v_habitat_tc_root = names(v_habitat_tc) |> str_remove("^log"),
  
  v_habitat_avant_tc_names = str_c("res", v_habitat_tc_root) |> syms(),
  
  v_habitat_avant_tc = v_habitat_tc |> as.list() |> set_names(v_habitat_avant_tc_names),
  
  v_habitat_avant = c(v_nuisances_avant,
                      v_habitat_avant_tc,
                      resnb = "Nombre de cohabitants"),
  
  v_caract = c(v_demo,
               v_age,
               v_religion,
               v_politisation,
               v_habitat_avant),
  
  ### Pratiques-------
  
  # Ce sont les 15 variables actives de l'ACM
  
  v_participation = list(confin_1 = "Applaudir les soignants",
                         confin_2 = "Huer le gouvernement", 
                         confin_7 = "S'informer sur l'épidémie"),
  
  v_precautions = list(protecdom_1 = "Porter des gants",
                       protecdom_2 = "Porter un masque",
                       protecdom_4 = "S'écarter de 1 m",
                       protecdom_6 = "Se désinfecter les mains",
                       protecdom_8 = "Désinfecter les surfaces"),
  
  v_interdits = list(att = "Sortir avec l'attestation",
                     atttype = "Type d'attestation",
                     attheure = "Ruser avec l'attestation",
                     dist = "Dépasser le rayon",
                     sortie = "Fréquence des promenades",
                     sortiequi = "Sortir le plus souvent"),
  
  v_pratiques = c(v_interdits,
                  v_precautions,
                  v_participation,
                  vote = "Élections municipales"),
  
  ## Data---------
  
  vico_read =  vroom::vroom(here::here("data","VICO1-DATA-SOURCE.csv"),
                            locale = vroom::locale(decimal_mark = ",")) |> 
    janitor::clean_names() |> 
    filter(poids_init != 0) |> 
    rename(resquartier = resquart) |>
    select(id, poids_init,
           all_of(c(names(v_pratiques),
                    names(v_vecu),
                    names(v_caract))),
    ) |> 
    mutate(across(c(ends_with(paste("_", 1:9, sep = ""))),
                  ~ if_else(str_detect(.x, "^Oui"), TRUE, FALSE)),
           across(all_of(names(v_habitat_tc)),
                  ~ coalesce(., get(str_c("res", cur_column()) |> str_remove("log"))))
    ) |> 
    mutate(sortie = case_match(sortie,
                               c("Moins d'une fois par semaine",
                                 "Une fois par semaine environ",
                                 "Plusieurs fois par semaine, mais pas tous les jours",
                                 "Une seule fois par jour, tous les jours ou presque") 
                               ~ "Une fois par jour au plus",
                               "Plusieurs fois par jour, et cela tous les jours ou presque" 
                               ~ "Plusieurs fois par jour",
                               .default = sortie),
           vote = case_match(vote,
                             c("Oui, à mon bureau de vote",
                               "Oui, par procuration")
                             ~ "Voter",
                             "Non, principalement parce que je craignais pour ma santé et celle des autres en raison de l'épidémie"
                             ~ "S'abstenir (raisons sanitaires)",
                             "Non, principalement pour une ou plusieurs autres raisons"
                             ~ "S'abstenir (raisons diverses)",
                             .default = vote),
           sortiequi = case_match(sortiequi,
                                  "Seul(e) le plus souvent" 
                                  ~ "Seul(e)",
                                  "Avec une ou plusieurs autres personnes avec laquel(le) ou lesquel(le)s vous êtes confiné(e)"
                                  ~ "Avec cohabitant(s)",
                                  "Avec une ou plusieurs autres personnes que vous retrouvez à l'extérieur"
                                  ~ "Avec personnes de l'extérieur",
                                  "C'est variable, parfois seul(e) et parfois avec une ou plusieurs autres personnes" 
                                  ~ "C'est variable",
                                  .default = sortiequi)) |>
    mutate(temps = case_match(temps,
                              "Oui, je manque vraiment de temps libre"
                              ~ "Vraiment",
                              "Oui, je manque un peu de temps libre"
                              ~ "Un peu",
                              "Non, je ne manque pas vraiment de temps libre"
                              ~ "Pas vraiment",
                              "Non, je ne manque pas du tout de temps libre"
                              ~ "Pas du tout",
                              .default = temps),
           tele = case_match(tele,
                             "Entièrement sur un ou des lieux de travail ou d'études qui se situent à l'extérieur du logement où vous êtes confiné(e)"
                             ~ "Entièrement à l'extérieur",
                             "Principalement sur un ou des lieux de travail ou d'études à l'extérieur, mais en partie aussi dans le logement où vous ê"
                             ~ "Principalement à l'extérieur",
                             "Principalement dans le logement où vous êtes confiné(e), mais en partie aussi sur un ou des lieux de travail ou d'études"
                             ~ "Principalement à domicile",
                             "Entièrement dans le logement où vous êtes confiné(e)"
                             ~ "Entièrement à domicile",
                             c("Je ne travaille pas", NA)
                             ~ "Ne travaille pas",
                             .default = tele),
           control = case_match(control,
                                "Non, jamais" ~ "Jamais",
                                "Oui, une seule fois" ~ "Une fois",
                                "Oui, plusieurs fois" ~ "Plusieurs fois",
                                .default = control),
           control = fct_relevel(control, "Jamais", "Une fois", "Plusieurs fois"),
           control_binaire = case_match(control,
                                        c("Une fois", "Plusieurs fois") ~ TRUE,
                                        "Jamais" ~ FALSE,
                                        NA ~ NA)) |>
    mutate(sexe = str_remove(sexe, "[A-z]{2,} "),
           sexe = str_to_sentence(sexe),
           pcs_rec9 = str_remove_all(pcs_rec9, "\\(.{1,2}\\)s"),
           pcs_rec9 = case_match(pcs_rec9,
                                 "Agriculteurs, agricultrices" 
                                 ~ "Agriculteur",
                                 "Elèves, étudiant" 
                                 ~ "Étudiant",
                                 "Professions intermédiaires" 
                                 ~ "Profession intermédiaire",
                                 "Cadres et professions intellectuelles supérieures"
                                 ~ "Cadre ou intellectuel",
                                 "Autres inactif"
                                 ~ "Autre inactif",
                                 .default = pcs_rec9),
           pcs_rec9 = fct_relevel(pcs_rec9, "Cadre ou intellectuel"),
           pcs_rec9 = fct_relevel(pcs_rec9, "Autre inactif", after = Inf)) |>
    labelled::set_variable_labels(.labels = c(v_pratiques, v_vecu, v_caract)),
  
  ## CAH------
  
  ### Vico ACM-----------
  
  vico_acm = vico_read |> 
    column_to_rownames("id") |>
    select(all_of(names(v_pratiques))) |>
    mutate(across(everything(), ~ paste(cur_column(), .x))),
  
  ### Fréquences----------
  
  frequences = vico_acm |>
    map_df(~  questionr::wtd.table(.x, weights = vico_read$poids_init) |>
             questionr::freq(sort = "", digits = 0, valid = FALSE), .id = "variable") |>
    rownames_to_column("modalite") |>
    rename(effectif = n,
           pourcentage = "%") |>
    relocate(variable),
  
  ### Modalités à exclure-------
  
  to_exclude = frequences |>
    filter(pourcentage < 2.2 | str_detect(modalite, " NA$") == TRUE),
  
  ### ACM & CAH-------
  
  cah_vico = vico_acm |>
    FactoMineR::MCA(excl = to_exclude$modalite,
                    ncp = 6,
                    row.w = vico_read$poids_init,
                    graph = FALSE) |>
    FactoMineR::HCPC(nb.clust = 6, nb.par = 50, graph = FALSE),
  
  vico_clust = cah_vico[["data.clust"]] |>
    rownames_to_column(var = "id") |>
    mutate(id = as.numeric(id)) |>
    select(id, clust),
  
  # Jointure -----
  
  vico = left_join(vico_read, vico_clust, by = "id")
  
)
