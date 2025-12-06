# This script will produce a rds that calculates the Multirace, Multiple Races, People of Color, and Single-race households total rows
# table detail types: Detail, Dichot, and Single 

library(tidyverse)
library(magrittr)

# retrieve data
source("household-count/01-hh-count-race-get-data.R")

race_vars <- c("PRACE", "ARACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# functions ----
# aggregating summary total rows, differentiating between Harvard and PSRC methods of grouping r/e categories
cat_multirace_psrc <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_multirace")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multirace*|^MNAW*|^Two.*"), "Multirace PSRC", "Not Multirace PSRC"))
}

cat_single_race_psrc <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_single")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multirace*|^MNAW*|^Two.*"), "Not single race PSRC", "Single race PSRC"))
}

cat_multirace_harvard <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_multirace")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multirace*|^MNAW*"), "Multirace Harvard", "Not Multirace Harvard"))
}

cat_single_race_harvard <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_single")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multirace*|^MNAW*"), "Not single race Harvard", "Single race Harvard"))
}

cat_poc <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_poc")
  df |> mutate({{newcol}} := ifelse(!c(.data[[race_col]] %in% c("Total", "White", NA)), "People of color", "Not People of Color"))
}

create_total_counts <- function(raw_pums) {
  
  # add labeling columns
  dl <- reduce2(race_vars, list("p", "a", "h"), cat_multirace_psrc, .init = raw_pums)
  dl <- reduce2(race_vars, list("p", "a", "h"), cat_single_race_psrc, .init = dl)
  dl <- reduce2(race_vars, list("p", "a", "h"), cat_multirace_harvard, .init = dl)
  dl <- reduce2(race_vars, list("p", "a", "h"), cat_single_race_harvard, .init = dl)
  dl <- reduce2(race_vars, list("p", "a", "h"), cat_poc, .init = dl)
  
  group_vars <- str_subset(colnames(dl$variables), ".*t_.*")
  
  # calc medians
  main_df <- NULL
  for(var in group_vars) {
    count_reg <- psrc_pums_count(dl, 
                                 group_vars=var, 
                                 incl_na=FALSE, 
                                 rr=TRUE)
    
    # extract record that's not Total and ^Not
    cats <- str_subset(unique(count_reg[[var]]), "^Total|^Not.*")
    count_reg <- count_reg |>
      filter(!(.data[[var]] %in% cats))
    
    count_cnty <- psrc_pums_count(dl, 
                                  group_vars=c("COUNTY", var), 
                                  incl_na=FALSE, 
                                  rr=TRUE) |>
      filter(COUNTY != "Region")
    
    count_cnty <- count_cnty |>
      filter(!(.data[[var]] %in% cats))
    
    # extract identifiers (race column type)
    rt <- str_extract(var, "^.*(?=_)")
    rt_name <- switch(rt, "acat" = "ARACE", "pcat" = "PRACE", "hcat" = "HRACE")
    
    # assemble and rename var to generic colnames and add new column to identify type of raw table
    rs <- bind_rows(count_reg, count_cnty) |>
      mutate(race_type = rt_name) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |>
      arrange(COUNTY)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs, main_df <- bind_rows(main_df, rs))
  }
  
  return(main_df)
}

# create total counts ----

all_dfs <- map2(list(pums_raw_hh_mrdetail,
                     pums_raw_hh_mrdichot,
                     pums_raw_hh_mrsingle), 
                table_types,
                ~create_total_counts(.x) |> mutate(table_type = .y)) 

all_dfs <- reduce(all_dfs, bind_rows)

all_dfs <- all_dfs |> 
  mutate(race = paste("Total", race))

saveRDS(all_dfs, "household-count/data/total-counts-df.rds")
# readRDS("household-count/data/total-counts-df.rds")
