# This script will calculate the 'Total Median of Median values' for Multirace, Multiple Races, and People of Color by the
# multiple race facets: Detail, Dichot, and Single 

library(tidyverse)
library(psrccensus)
library(magrittr)

# retrieve data
source("median-income/01-med-inc-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")

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

# detail ----

# add labeling new columns
dl <- reduce2(race_vars, list("a", "p", "h"), cat_multirace, .init = pums_raw_hh_mrdetail) 
dl <- reduce2(race_vars, list("a", "p", "h"), cat_multiple_race, .init = dl)
dl <- reduce2(race_vars, list("a", "p", "h"), cat_poc, .init = dl)

group_vars <- str_subset(colnames(dl$variables), ".*t_.*")

for(var in group_vars) {
  med_reg <- psrc_pums_median(dl, 
                           stat_var = "HINCP",
                           group_vars = var, 
                           incl_na = FALSE, 
                           rr = TRUE)
  
  # extract record that's not Total and ^Not
  cats <- str_subset(unique(med_reg[[var]]), "^Total|^Not.*")
  med_reg <- med_reg |> 
    filter(!(.data[[var]] %in% cats))
  
  med_cnty <- psrc_pums_median(dl, 
                            stat_var = "HINCP",
                            group_vars = c("COUNTY", var), 
                            incl_na = FALSE, 
                            rr = TRUE) |> 
    filter(COUNTY != "Region")
  
  med_cnty <- med_cnty |> 
    filter(!(.data[[var]] %in% cats))
  
  # assemble and rename var to generic colname and add new column to identify type of raw table
  rs <- bind_rows(med_reg, med_cnty) |> 
    arrange(DATA_YEAR, COUNTY) |> 
    mutate(table_type = "detail")
    rename(total_type = var)
}






