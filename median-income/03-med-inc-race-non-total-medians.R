# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("median-income/01-med-inc-race-get-data.R")
# source("C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/median-income/01-med-inc-race-get-data.R")

# remove functions in environment created in previous scripts
rm(list = lsf.str(envir = .GlobalEnv), envir = .GlobalEnv)

race_vars <- c("PRACE", "ARACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    med_reg <- psrc_pums_median(dl,
                                stat_var = "HINCP",
                                group_vars = var,
                                incl_na = FALSE,
                                rr = TRUE)
    
    med_cnty <- psrc_pums_median(dl,
                                 stat_var = "HINCP",
                                 group_vars = c("COUNTY", var),
                                 incl_na = FALSE,
                                 rr = TRUE) |>
      filter(COUNTY != "Region")
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(med_reg, med_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs, main_df <- bind_rows(main_df, rs))
  }
}

saveRDS(main_df, "median-income/data/non-total-medians-df.rds")
# saveRDS(all_dfs, "C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/median-income/data/non-total-median-df.rds")

