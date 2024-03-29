# Packages-----

library(targets)
library(tarchetypes)

# Options-----

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse", "readxl",
                            "lubridate", "ggrepel",
                            "sf", "countrycode", "tmap"))

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
           is.na(sub_region_2)) |>
    collect() |> 
    mutate(date = ymd(date),
           wb = countrycode(country_region, origin = "country.name", destination = "wb")) |>
    select(wb, date, ends_with("_baseline")),
  
  # Excess mortality------
  
  excess_mortality = read_rds(excess_mortality_file) |>
    mutate(date = ymd(date),
           wb = countrycode(location, origin = "country.name.en", destination = "wb")) |>
    select(wb,
           date,
           p_scores_all_ages,
           deaths_2020_all_ages,
           projected_deaths_since_2020_all_ages,
           excess_proj_all_ages),
  
  # Police Europe--------
  
  police_europe = read_rds(police_europe_file) |>
    pivot_wider(names_from = c(sex, isco08, unit), values_from = "values",
                names_repair = janitor::make_clean_names) |>
    complete(geo, time) |>
    mutate(geo = if_else(geo %in% c("Scotland", "England and Wales", "Northern Ireland (UK)"),
                         "United Kingdom", geo)) |>
    group_by(geo, time) |>
    summarise(across(ends_with("number"), ~ sum(.x, na.rm = F)), .groups = "drop") |>
    mutate(geo = str_replace(geo, "Türkiye", "Turkey"),
           geo = if_else(str_detect(geo, "Germany"), "Germany", geo),
           geo = if_else(str_detect(geo, "Kosovo"), "Republic of Kosovo", geo),
           year = year(time),
           wb = countrycode(geo, origin = "country.name", destination = "wb")) |>
    select(wb, year,
           ends_with("number") &
             contains("police")),
  
  wb_police_europe_pcmh = wb |>
    left_join(police_europe) |>
    mutate(policiers_pcm_habitants = total_police_officers_number / population * 100000) |>
    group_by(wb) |>
    mutate(policiers_pcm_habitants = mean(policiers_pcm_habitants, na.rm = TRUE)) |>
    filter(year == 2020),
  
  # AGR-------
  
  agr = read_rds(agr_file) |>
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
    left_join(agr) |>
    left_join(google) |>
    left_join(apple) |>
    left_join(excess_mortality) |>
    filter(between(date, date_beginn, date_end),
           !is.na(wb)) |>
    distinct(wb, date, .keep_all = TRUE) |>
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
  
  stringency_index_plot = world_summarise |>
    
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
           !is.na(policiers_pcm_habitants)) |>
    
    mutate(pays = fct_reorder(pays, policiers_pcm_habitants)) |>
    
    ggplot2::ggplot(aes(y = pays, x = policiers_pcm_habitants)) +
    
    geom_bar(stat = "identity"),
  
  ## Fariss & Parks--------
  
  fariss_parks_plot = world_summarise |>
    
    filter(!is.na(policiers_pcm_habitants),
           !pays %in% c("Turquie", "Luxembourg")) |>
    
    ggplot(aes(x = hr_fariss, y = parks_percent_change_from_baseline)) +
    
    geom_point() +
    
    geom_text_repel(aes(label = pays), family = geom_fontfamily, size = 3) +
    
    scale_x_continuous(limits = c(1, 4)) +
    
    labs(title = "Plus les Étas européens respectaient le droit à l'intégrité physique,\nmoins ils ont enfermé leur population",
         x = "Droit à l'intégrité physique 2019 (Indice synthétique)",
         y = "Fréquentation des espaces verts du 1er mars au 1er juin\npar rapport à janvier 2020 (%)",
         caption = "Données : Latent Human Rights Protection Scores Version 4 & Google Mobility Reports"),
  
  ## Police & Parks---------
  
  police_parks_plot = world_summarise |>
    ggplot(aes(x = policiers_pcm_habitants, y = parks_percent_change_from_baseline)) +
    geom_point() +
    geom_text_repel(aes(label = pays), family = geom_fontfamily, size = 3) +
    scale_x_continuous(limits = c(130, 510)) +
    labs(title = "Plus les États européens comptaient de policiers par habitant,\nplus ils ont enfermé leur population",
         x = "Policiers et assimilés pour 100 000 habitants",
         y = "Fréquentation des espaces verts du 1er mars au 1er juin\npar rapport à janvier 2020 (%)",
         caption = "Données : Eurostat & Google Mobility Reports"),
  
  ## AGR & Police-----------
  
  agr_police_plot = world_summarise |>
    filter(!is.na(mode_least_strict_day_april),
           !is.na(policiers_pcm_habitants)) |>
    mutate(mode_least_strict_day_april = str_wrap(mode_least_strict_day_april, 5),
           mode_least_strict_day_april = fct_reorder(mode_least_strict_day_april,
                                                     policiers_pcm_habitants)) |>
    ggplot(aes(x = mode_least_strict_day_april,
               y = policiers_pcm_habitants)) +
    geom_boxplot(varwidth = TRUE) +
    geom_label_repel(aes(label = pays), family = geom_fontfamily, size = 2.5) +
    labs(title = "La concentration policière augmente avec le niveau des restrictions",
         subtitle = "La France est dans le petit groupe des pays à attestation",
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
    labs(title = "Densité de policiers municipaux") +
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
    labs(title = "Prévalence de la verbalisation",
         fill = "Pour 1000 adultes",
         caption = "Sources : ANTAI, INSEE"),
  
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
         caption = "Données : Presse (pour les contrôles), ANTAI (pour les verbalisations)")
  
)
