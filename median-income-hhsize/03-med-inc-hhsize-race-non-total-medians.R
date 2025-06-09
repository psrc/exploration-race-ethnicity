# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("median-income-hhsize/01-med-inc-hhsize-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    med_reg <- psrc_pums_median(dl,
                                stat_var = "HINCP",
                                group_vars = c(var,"hhsz_binary"),
                                incl_na = FALSE,
                                rr = TRUE) 
    
    med_cnty <- psrc_pums_median(dl,
                                 stat_var = "HINCP",
                                 group_vars = c("COUNTY",var,"hhsz_binary"),
                                 incl_na = FALSE,
                                 rr = TRUE) |> 
      filter(COUNTY != "Region")
    
    # extract from tables below where: XRace == "Total" 
    med_reg2 <- psrc_pums_median(dl,
                                 stat_var = "HINCP",
                                 group_vars = c("hhsz_binary", var),
                                 incl_na = FALSE,
                                 rr = TRUE) 
    
    med_cnty2 <- psrc_pums_median(dl,
                                  stat_var = "HINCP",
                                  group_vars = c("COUNTY","hhsz_binary", var),
                                  incl_na = FALSE,
                                  rr = TRUE)|> 
      filter(COUNTY != "Region")
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(med_reg, med_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    rs2 <- bind_rows(med_reg2, med_cnty2) |> 
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY) |> 
      filter(race == 'Total' & hhsz_binary != 'Total')
    
    rs3 <- bind_rows(rs, rs2) |> 
      arrange(COUNTY, race, hhsz_binary)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs3, main_df <- bind_rows(main_df, rs3))
  }
}


saveRDS(main_df, "median-income-hhsize/data/non-total-counts-df.rds")
# saveRDS(main_df, "median-income-hhsize/data/non-total-counts-df-singleperson.rds")
# saveRDS(main_df, "median-income-hhsize/data/non-total-counts-df-multiperson.rds")

# readRDS("median-income-hhsize/data/non-total-counts-df-singleperson.rds")