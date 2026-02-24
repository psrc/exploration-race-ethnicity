# This script will produce a rds that compiles non-total median income by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("household-size/01-hh-size-race-get-data.R")

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
    count_reg <- psrc_pums_count(dl, 
                                 group_vars=c("hhsz_binary",var), 
                                 incl_na=FALSE, 
                                 rr=TRUE) #%>%
    # filter(hhsz_binary == "single-person" |
    #          hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    count_cnty <- psrc_pums_count(dl, 
                                  group_vars=c("COUNTY","hhsz_binary",var), 
                                  incl_na=FALSE, 
                                  rr=TRUE) %>%
      filter(COUNTY != "Region") #%>% 
      # filter(hhsz_binary == "single-person" |
      #          hhsz_binary == "multi-person") # need to filter because added 'Total' rows with added variable
    
    # add nulls for missing county/race values (just in case)
    completed_count_cnty <- count_cnty %>%
      complete(COUNTY = c("King", "Kitsap", "Pierce", "Snohomish"), #can't use unique() b/c "Region" and "Total"
               !!sym(var) := c("American Indian or Alaskan Native", 
                               "Asian",
                               "Black or African American",
                               "Hispanic or Latino",
                               "Native Hawaiian or Pacific Islander",
                               "Some Other Race",
                               "Two or More Races",
                               "White"), #can't use unique() b/c "Total"
               hhsz_binary = unique(count_cnty$hhsz_binary),
               DATA_YEAR = unique(count_cnty$DATA_YEAR))
    
    # extract from tables below where: XRace == "Total" 
    count_reg2 <- psrc_pums_count(dl, 
                                  group_vars=c("hhsz_binary",var), 
                                  incl_na=FALSE, 
                                  rr=TRUE)
    
    count_cnty2 <- psrc_pums_count(dl, 
                                   group_vars=c("COUNTY",var, "hhsz_binary"), 
                                   incl_na=FALSE, 
                                   rr=TRUE)|> 
      filter(COUNTY != "Region")
    
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(count_reg, count_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    # rs2 <- bind_rows(count_reg2, count_cnty2) |>
    #   mutate(race_type = var,
    #          table_type = ttype) |>
    #   mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
    #   rename(race = var) |>
    #   arrange(COUNTY) |>
    #   filter(race == 'Total' & hhsz_binary != 'Total')
    
    # rs3 <- bind_rows(rs, rs2) |>
    #   arrange(COUNTY, race, hhsz_binary)
    
    rs3 <- rs |>
      arrange(COUNTY, race, hhsz_binary)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs3, main_df <- bind_rows(main_df, rs3))
  }
}

saveRDS(main_df, "household-size/data/non-total-counts-df.rds")
# test2 <- readRDS("household-size/data/non-total-counts-df.rds")
