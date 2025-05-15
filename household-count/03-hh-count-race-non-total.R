# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("household-count/01-hh-count-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    count_reg <- psrc_pums_count(dl, 
                                 group_vars=var, 
                                 incl_na=FALSE, 
                                 rr=TRUE)
    
    count_cnty <- psrc_pums_count(dl, 
                                  group_vars=c("COUNTY", var), 
                                  incl_na=FALSE, 
                                  rr=TRUE) |>
      filter(COUNTY != "Region")
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(count_reg, count_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs, main_df <- bind_rows(main_df, rs))
  }
}

saveRDS(main_df, "household-count/data/non-total-counts-df.rds")

