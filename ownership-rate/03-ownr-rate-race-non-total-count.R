# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("ownership-rate/01-ownr-rate-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    count_reg <- psrc_pums_count(dl,
                                 group_vars = c(var, "OWN_RENT"),
                                incl_na = FALSE,
                                rr = TRUE) |>
      filter(OWN_RENT == "Owned")
    
    count_cnty <- psrc_pums_count(dl,
                                 group_vars = c("COUNTY", var, "OWN_RENT"),
                                 incl_na = FALSE,
                                 rr = TRUE) |>
      filter(COUNTY!="Region", OWN_RENT == "Owned")
    
    # extract from tables below where: XRace == "Total" 
    count_reg2 <- psrc_pums_count(dl,
                                 group_vars = c("OWN_RENT", var),
                                 incl_na = FALSE,
                                 rr = TRUE) |>
      filter(OWN_RENT == "Owned")
    
    count_cnty2 <- psrc_pums_count(dl,
                                  group_vars = c("COUNTY", "OWN_RENT", var),
                                  incl_na = FALSE,
                                  rr = TRUE) |>
      filter(COUNTY!="Region", OWN_RENT == "Owned")
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(count_reg, count_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("King", "Kitsap", "Pierce", "Snohomish", "Region"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    rs2 <- bind_rows(count_reg2, count_cnty2) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("King", "Kitsap", "Pierce", "Snohomish", "Region"))) |>
      rename(race = var) |> 
      arrange(COUNTY) |> 
      filter(race == 'Total')
    
    rs3 <- bind_rows(rs, rs2) |> 
      arrange(COUNTY, race)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs3, main_df <- bind_rows(main_df, rs3))
  }
}

saveRDS(main_df, "ownership-rate/data/non-total-count-df.rds")

readRDS("ownership-rate/data/non-total-count-df.rds")
