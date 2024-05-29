## Fonctions--------

### Recodages------

lag_indice_in_string <- function(string) {
  
  laged_indice <- parse_number(string) - 1
  
  string_without_indice <- str_remove(string, "\\d")
  
  str_c(string_without_indice, laged_indice)
  
}

## Dendogramme--------

name_groups <- function(data) {
  
  data |>
    mutate(clust = case_when(clust == 1 ~ "Exemplaires",
                             clust == 2 ~ "Protestataires",
                             clust == 3 ~ "Claustrés",
                             clust == 4 ~ "Insouciants",
                             clust == 5 ~ "Légalistes",
                             clust == 6 ~ "Réfractaires"))
}

order_groups <- function(x) {
  
  k <- length(unique(x[["data.clust"]]$clust))
  
  clust.hcpc <- as.numeric(x$data.clust$clust)
  
  clust.cutree <- dendextend:::cutree(x$call$t$tree, k = k,
                                      order_clusters_as_data = FALSE)
  
  idx <- order(as.numeric(names(clust.cutree)))
  
  clust.cutree <- clust.cutree[idx]
  
  tbl <- table(clust.hcpc, clust.cutree)
  
  lbls <- apply(tbl, 2, which.max) 
  
  tibble(clust = lbls) %>%
    name_groups() %>%
    pull()
  
}

visualize_dendrogram <- function(x) {
  
  k <- length(unique(x[["data.clust"]]$clust))
  
  col_groups <- rcartocolor::carto_pal(n = k, name = "Antique")
  
  x$call$t$tree %>%
    
    dendextend::color_branches(k = k, col = col_groups, groupLabels = order_groups(x)) %>%
    
    # set("branches_lwd", 2) %>%
    
    set("labels", NULL)
  
  # plot(horiz = FALSE,
  #      main = "",
  #      ylab = "")
  
}

## Tableaux------

table_to_tibble <- function(table) {
  
  table |> 
    as_tibble(.name_repair = janitor::make_clean_names) |>
    rename(modalite = x,
           clust = x_2) 
}

bind_chi_prop <- function(x) {
  
  weighted_table = questionr::wtd.table(vico[[x]], vico$clust, weights = vico$poids_init)
  
  chi = weighted_table |> 
    questionr::chisq.residuals() |> # Pearson residuals
    table_to_tibble() |> 
    rename(chi = n)
  
  prop = weighted_table |> 
    questionr::cprop() |> 
    table_to_tibble() |> 
    rename(prop = n)
  
  left_join(prop, chi, by = c("modalite", "clust"))
  
}

map_chi_prop <- function(labelled_list_of_variables) {
  
  names(labelled_list_of_variables) |> 
    
    map(bind_chi_prop) |> 
    
    set_names(labelled_list_of_variables) |> 
    
    list_rbind(names_to = "variable") |> 
    
    pivot_wider(names_from = clust, values_from = c(chi, prop))
}

preserve_order_when_arranging <- function(data, x) {
  
  data |> 
    
    mutate(r = row_number()) |> 
    
    group_by(variable) |> 
    
    mutate(index = mean(r)) |> 
    
    arrange(desc({{ x }}), .by_group = TRUE) |> 
    
    arrange(index) |> 
    
    select(-r, -index) |> 
    
    ungroup()
}

name_clust <- function(x) {
  
  case_match(x,
             "1" ~ "Exemplaires",
             "2" ~ "Protestataires",
             "3" ~  "Claustrés",
             "4" ~  "Insouciants",
             "5" ~  "Légalistes",
             "6" ~  "Réfractaires",
             .default = x)
}

shape_hux <- function(data, clusters = 1:6) {
  
  data |>
    
    filter(modalite != FALSE,
           modalite != "Total") |> 
    
    mutate(across(starts_with("chi_"), ~ case_when(.x > 2 ~ "+",
                                                   .x < -2 ~ "-",
                                                   .default = "=")),
           across(starts_with("prop_"), round),
           across(starts_with("prop_") & !ends_with("_Ensemble"),
                  ~ str_c(.x, get(str_c("chi_", cur_column() |> parse_number()))))
    ) |> 
    
    mutate(modalite = str_replace(modalite, "TRUE", variable),
           variable = case_match(variable,
                                 as.character(v_precautions) ~ "Précautions",
                                 as.character(v_participation) ~ "Participation",
                                 as.character(v_politisation) ~ "Politisation",
                                 as.character(v_emotions) ~ "Émotions",
                                 as.character(v_travail) ~ "Travail",
                                 as.character(v_nuisances) ~ "Nuisances",
                                 as.character(tail(v_habitat_tc, n = 4)) ~ "Accès à l'extérieur",
                                 .default = variable)) |> 
    
    preserve_order_when_arranging(prop_Ensemble) |>
    
    select(variable, modalite, prop_Ensemble, paste0("prop_", clusters)) |> 
    
    relocate(prop_Ensemble, .after = modalite) |> 
    
    rename_with(~ str_remove(.x, "prop_"), .cols = starts_with("prop_")) |> 
    
    rename_with(name_clust) |>
    
    relocate(variable, modalite, Ensemble, any_of(ordered_clust)) |> 
    
    rename(" " = variable,
           "  " = modalite) |> 
    
    huxtable() |>
    
    map_bold(col = 4:last_col(), fn = by_regex("+" = TRUE,
                                               .grepl_args = list(fixed = TRUE))) |> 
    
    map_italic(col = 4:last_col(), fn = by_regex("-" = TRUE,
                                                 .grepl_args = list(fixed = TRUE))) |> 
    
    mutate(across(!1:3, ~ str_remove(.x, fixed("+"))),
           across(!1:3, ~ str_remove(.x, fixed("-"))),
           across(!1:3, ~ if_else(str_detect(.x, fixed("=")), " ", .x))) |> 
    
    set_align(row = everywhere, col = 4:last_col(), "center") |> 
    
    set_italic(row = everywhere, col = 2) |> 
    
    column_to_header(col = 1)
  
}
