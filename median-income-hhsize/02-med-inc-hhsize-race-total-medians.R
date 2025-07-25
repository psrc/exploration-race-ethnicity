# This script will produce a rds that calculates the 'Total Median of Median values' for Multirace, Multiple Races, and People of Color by the
# table detail types: Detail, Dichot, and Single 

library(tidyverse)
library(magrittr)

# retrieve data
source("median-income-hhsize/01-med-inc-hhsize-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# functions ----

cat_multirace <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_multi")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multi.*"), "Multirace", "Not Multirace"))
}

cat_multiple_race <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_multiple")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multi.*|^Two.*"), "Multiple Races", "Not Multiple Races"))
}

cat_poc <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_poc")
  df |> mutate({{newcol}} := ifelse(!c(.data[[race_col]] %in% c("Total", "White alone", NA)), "People of Color", "Not People of Color"))
}

create_total_medians <- function(raw_pums) {
  
  # add labeling columns
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_multirace, .init = raw_pums)
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_multiple_race, .init = dl)
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_poc, .init = dl)
  
  group_vars <- str_subset(colnames(dl$variables), ".*t_.*")
  
  # calc medians
  main_df <- NULL
  for(var in group_vars) {
    med_reg <- psrc_pums_median(dl,
                                stat_var = "HINCP",
                                group_vars = c(var,"hhsz_binary"),
                                incl_na = FALSE,
                                rr = TRUE)
    
    # extract record that's not Total and ^Not
    cats <- str_subset(unique(med_reg[[var]]), "^Total|^Not.*")
    
    med_reg <- med_reg |>
      filter(!(.data[[var]] %in% cats))
    
    med_cnty <- psrc_pums_median(dl,
                                 stat_var = "HINCP",
                                 group_vars = c("COUNTY",var,"hhsz_binary"),
                                 incl_na = FALSE,
                                 rr = TRUE) |> 
      filter(COUNTY != "Region")
    
    med_cnty <- med_cnty |>
      filter(!(.data[[var]] %in% cats))
    
    # extract identifiers (race column type)
    rt <- str_extract(var, "^.*(?=_)")
    rt_name <- switch(rt, "acat" = "ARACE", "pcat" = "PRACE", "hcat" = "HRACE")
    
    # assemble and rename var to generic colnames and add new column to identify type of raw table
    rs <- bind_rows(med_reg, med_cnty) |>
      mutate(race_type = rt_name) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |>
      arrange(COUNTY)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs, main_df <- bind_rows(main_df, rs))
  }
  
  return(main_df)
}

# create total medians ----

all_dfs <- map2(list(pums_raw_hh_mrdetail,
                    pums_raw_hh_mrdichot,
                    pums_raw_hh_mrsingle), 
                table_types,
                ~create_total_medians(.x) |> mutate(table_type = .y)) 

all_dfs <- reduce(all_dfs, bind_rows)

all_dfs <- all_dfs |> 
  mutate(race = paste("Total", race))

saveRDS(all_dfs, "median-income-hhsize/data/total-counts-df.rds")

# readRDS("median-income-hhsize/data/total-counts-df-multiperson.rds")
