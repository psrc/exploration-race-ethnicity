# This script will produce a rds that calculates the Multirace, Multiple Races, People of Color, and Single-race households total rows
# table detail types: Detail, Dichot, and Single 

library(tidyverse)
library(magrittr)

# retrieve data
source("renter-cost-burden/01-renter-cost-burden-race-get-data.R")

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
  df |> mutate({{newcol}} := ifelse(!(.data[[race_col]] %in% c("Total", "White alone", NA)), "People of Color", "Not People of Color"))
}

cat_single_race <- function(df, race_col, x) {
  newcol <- paste0(x, "cat_single")
  df |> mutate({{newcol}} := ifelse(str_detect(.data[[race_col]], "^Multi.*"), "Not single-race", "Single-race"))
}

create_total_counts <- function(raw_pums) {
  
  # add labeling columns
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_multirace, .init = raw_pums)
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_multiple_race, .init = dl)
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_poc, .init = dl)
  dl <- reduce2(race_vars, list("a", "p", "h"), cat_single_race, .init = dl)
  
  group_vars <- str_subset(colnames(dl$variables), ".*t_.*")
  
  # calc counts
  main_df <- NULL
  for(var in group_vars) {
    count_reg <- psrc_pums_count(dl, 
                                 group_vars=c(var, "rent_burden"), 
                                 incl_na=FALSE, 
                                 rr=TRUE) %>%
      filter(rent_burden == "cost-burdened")
    
    # extract record that's not Total and ^Not
    cats <- str_subset(unique(count_reg[[var]]), "^Total|^Not.*")
    
    count_reg <- count_reg |>
      filter(!(.data[[var]] %in% cats))
    
    count_cnty <- psrc_pums_count(dl, 
                                  group_vars=c("COUNTY",var,"rent_burden"), 
                                  incl_na=FALSE, 
                                  rr=TRUE) %>%
      filter(rent_burden == "cost-burdened") %>% 
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
      arrange(COUNTY) |>
      filter(race != "cost-burdened")
    
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


saveRDS(all_dfs, "renter-cost-burden/data/total-counts-df.rds")

# readRDS("renter-cost-burden/data/total-counts-df.rds")
