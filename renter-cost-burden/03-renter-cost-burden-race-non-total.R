# This script will produce a rds that compiles non-total count values by geography, race, and table detail types

library(tidyverse)
library(magrittr)

# retrieve data
unloadNamespace("psrccensus")
source("renter-cost-burden/01-renter-cost-burden-race-get-data.R")

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

# compile non-aggregated totals (i.e not Multirace, Multiple Races, POC) ----

main_df <- NULL
for(ttype in table_types) {
  
  # find table object
  dl <- get(ls(pattern = ttype))
  
  for(var in race_vars) {
    count_reg <- psrc_pums_count(dl, 
                                 group_vars=c(var, "rent_burden"), 
                                 incl_na=FALSE, 
                                 rr=TRUE) %>%
      filter(rent_burden == "cost-burdened") 
    
    count_cnty <- psrc_pums_count(dl, 
                                  group_vars=c("COUNTY",var, "rent_burden"), 
                                  incl_na=FALSE, 
                                  rr=TRUE) %>%
      filter(rent_burden == "cost-burdened") %>% 
      filter(COUNTY != "Region")
    
    # extract from tables below where: XRace == "Total" 
    count_reg2 <- psrc_pums_count(dl, 
                                  group_vars=c("rent_burden",var), 
                                  incl_na=FALSE, 
                                  rr=TRUE) %>%
      filter(rent_burden == "cost-burdened") 
    
    count_cnty2 <- psrc_pums_count(dl, 
                                   group_vars=c("COUNTY","rent_burden",var), 
                                   incl_na=FALSE, 
                                   rr=TRUE) %>%
      filter(rent_burden == "cost-burdened") %>% 
      filter(COUNTY != "Region")
    
    
    # rename var to generic colnames to assemble and add new column to identify type of raw table
    rs <- bind_rows(count_reg, count_cnty) |>
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY)
    
    rs2 <- bind_rows(count_reg2, count_cnty2) |> 
      mutate(race_type = var,
             table_type = ttype) |>
      mutate(COUNTY = factor(COUNTY, levels = c("Region", "King", "Kitsap", "Pierce", "Snohomish"))) |>
      rename(race = var) |> 
      arrange(COUNTY) |> 
      filter(race == 'Total')
    
    rs3 <- bind_rows(rs, rs2) |> 
      arrange(COUNTY, race)
    
    # bind to main table
    ifelse(is.null(main_df), main_df <- rs3, main_df <- bind_rows(main_df, rs3))
  }
}

# check output
# main_df

saveRDS(main_df, "renter-cost-burden/data/non-total-counts-df.rds")

# readRDS("renter-cost-burden/data/non-total-counts-df-singleperson.rds") 




# # check against ACS -----
# acs_df <- get_acs_recs(geography ='county',
#                        table.names = 'DP04',
#                        years = 2023, # refers to the list of years that was set above
#                        acs.type = 'acs5')
# acs_rent <- acs_df %>%
#   filter(variable=="DP04_0141P"| #GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)!!Occupied units paying rent (excluding units where GRAPI cannot be computed)!!30.0 to 34.9 percent
#            variable=="DP04_0142P") #GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME (GRAPI)!!Occupied units paying rent (excluding units where GRAPI cannot be computed)!!35.0 percent or more
# 
# acs_rent_burdened <- acs_rent %>%
#   group_by(name) %>% #county name
#   summarise(estimate_percent=sum(estimate))
# 
# # check against PUMS -----
# get_data <- get_psrc_pums(5, dyear , "h", 
#                           c("HRACE", "NP", "GRPIP", "OWN_RENT"), # add any additional variables you may want
#                           dir = dir) # pull network archived copy because Census Bureau ftp site not allowing downloads
# get_data_ref <- get_data %>%
#   mutate(hhsz_binary = case_when(NP==1~"single-person",
#                                  NP>1~"multi-person"),
#          rent_burden = case_when(OWN_RENT =="Rented" & GRPIP<30  ~"not cost-burdened",
#                                  OWN_RENT =="Rented" & GRPIP>=30 ~"cost-burdened",
#                                  TRUE~NA))
# # all_pums <- psrc_pums_count(get_data_ref, 
# #                             group_vars=c("COUNTY","HRACE","hhsz_binary", "rent_burden"), 
# #                             incl_na=FALSE, 
# #                             rr=TRUE)
# # cost_burdened_test <- all_pums %>% 
# #   filter(rent_burden=="cost-burdened")
# 
# # to just look at county numbers, without race or household size (because not available in ACS table)
# compare_to_acs <- psrc_pums_count(get_data_ref, 
#                                   group_vars=c("COUNTY","rent_burden"), 
#                                   incl_na=FALSE, 
#                                   rr=TRUE) %>% 
#   filter(rent_burden=="cost-burdened")
