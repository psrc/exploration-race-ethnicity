# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("ownership-rate-hhsize/01-ownr-rate-hhsize-race-get-data.R")

# remove functions in environment created in previous scripts
rm(list = lsf.str(envir = .GlobalEnv), envir = .GlobalEnv)

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    count_reg <- psrc_pums_count(dl,
                                 group_vars = c(var,"hhsz_binary","OWN_RENT"),
                                incl_na = FALSE,
                                rr = TRUE) |>
      filter(OWN_RENT == "Owned") |>
     filter(hhsz_binary == "single-person" |
            hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    count_cnty <- psrc_pums_count(dl,
                                 group_vars = c("COUNTY", var,"hhsz_binary","OWN_RENT"),
                                 incl_na = FALSE,
                                 rr = TRUE) |>
      filter(COUNTY!="Region", OWN_RENT == "Owned") |>
      filter(hhsz_binary == "single-person" |
               hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    # extract from tables below where: XRace == "Total"
    count_reg2 <- psrc_pums_count(dl,
                                 group_vars = c("hhsz_binary","OWN_RENT",var),
                                 incl_na = FALSE,
                                 rr = TRUE) |>
      filter(OWN_RENT == "Owned") |>
      filter(hhsz_binary == "single-person" |
               hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    count_cnty2 <- psrc_pums_count(dl,
                                  group_vars = c("COUNTY","hhsz_binary","OWN_RENT",var),
                                  incl_na = FALSE,
                                  rr = TRUE) |>
      filter(COUNTY!="Region", OWN_RENT == "Owned") |>
      filter(hhsz_binary == "single-person" |
               hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    
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

saveRDS(main_df, "ownership-rate-hhsize/data/non-total-counts-df.rds")

readRDS("ownership-rate-hhsize/data/non-total-counts-df.rds")
