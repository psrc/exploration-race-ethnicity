# TITLE: Exploring Household Race/Ethnicity Categorization via new PSRCcensus Categories
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS PUMS
# DATE MODIFIED: 02.18.2024
# AUTHORS: Eric Clute and Mary Richards


# Set up libraries and variables ------------------------------

library(tidyverse)
library(magrittr)
library(openxlsx)
library(data.table)

race_vars <- c("ARACE", "PRACE", "HRACE")
dyear <- 2023
library_loc <- "C:/Users/CLam/AppData/Local/R/win-library/4.4"
# output_loc <- "T:/60day-TEMP/Mary/Other/HH_race_ethnicity"
dir <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds" # added to access archived copy of main brance of psrccensus

# You will need to do this initial step to install both branches so that you can retrieve each PUMS data object (since the correct race/ethnicity variable categories are distinct). It will require you to create 2 folders within the R directory (library_loc) to save the corresponding data: "psrccensus_mrdetail" and "multiracial_dichotomy"

# remotes::install_github("psrc/psrccensus", ref="multiracial_detail",
#                         lib= file.path(library_loc,"psrccensus_mrdetail"),
#                         # lib="C:/Users/eclute/Documents/local_r_packages/psrccensus_mrdetail",
#                         force = TRUE)
# remotes::install_github("psrc/psrccensus", ref="multiracial_dichotomy",
#                         lib= file.path(library_loc,"psrccensus_mrdichot"),
#                         # lib="C:/Users/eclute/Documents/local_r_packages/psrccensus_mrdichot",
#                         force = TRUE)


# Retrieve data ---------------------------------
library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdetail"))
pums_raw_hh_mrdetail <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir=dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdichot"))
pums_raw_hh_mrdichot <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir=dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus)
pums_raw_hh_mrsingle <- get_psrc_pums(5, dyear , "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir=dir) # pull network archived copy because Census Bureau ftp site not allowing downloads


# Helper functions ------------------------------
## 1a. This function creates one long data frame with each of the geographies, 
# for the race_var specified (PRACE, HRACE, or ARACE) with count, count_moe, reliability, share, and share_moe
ctyreg_pums_median <- function(group_vars, pums_data){
  rs      <- list()
  rs[[1]] <- psrc_pums_median(pums_data, 
                              stat_var = "HINCP",
                              group_vars=group_vars, 
                              incl_na=FALSE, 
                              rr=TRUE)
  rs[[2]] <- psrc_pums_median(pums_data, 
                              stat_var = "HINCP",
                              group_vars=c("COUNTY", group_vars), 
                              incl_na=FALSE, 
                              rr=TRUE) %>%
    filter(COUNTY!="Region")
  rs %<>% data.table::rbindlist() %>% 
    arrange(DATA_YEAR, COUNTY)
  return(rs)
}

## 1b. This function applies function 1a and creates a list of 3 long data tables - one for each of the race_var (PRACE, HRACE, or ARACE) with count, count_moe, reliability, share, and share_moe values; it iterates through the race_var (PRACE, HRACE, or ARACE), without having to specify each race_var separately in 1a
bundle_count <- purrr::partial(sapply, FUN=ctyreg_pums_median, simplify=FALSE, USE.NAMES=TRUE)

## 2a. This function creates 1 data frame with the count, reliability, and share for the race_var (PRACE, HRACE, or ARACE) specified - it renames fields so that they can be joined once the tables for the other race_var data frames are generated
# geography_count <- function(df, race_vars, geography){
#   detail <- df[[race_vars]] %>%
#     filter(COUNTY==geography) %>%
#     dplyr::select(DATA_YEAR, COUNTY, race_vars, count, reliability, share) %>%
#     rename_with(.cols=count:share, function(x){paste0(x, "_", race_vars)}) %>%
#     rename("RACE"=race_vars)
# 
#   added_total <- detail %>%
#     bind_rows(
#       detail %>%
#         filter(grepl('Multi', RACE)) %>% #'Multiracial...' in detail/dichot and 'Multiple Races' in single
#         summarise_at(vars(contains(c("count_","share_"))), sum) %>%
#         mutate(RACE = "Total Multiracial"))
# 
#   return(added_total)
# 
# }

geography_count <- function(df, race_vars, geography){
  detail <- df[[race_vars]] %>%
    filter(COUNTY==geography) %>%
    dplyr::select(race_vars, HINCP_median, reliability) %>%
    rename_with(.cols=HINCP_median:reliability, function(x){paste0(x, "_", race_vars)}) %>%
    rename("RACE"=race_vars)
  
  # # creating rows that include the 'totals' for different groupings
  # added_total <- detail %>%
  #   bind_rows(
  #     detail %>%
  #       filter(grepl('Multi', RACE)) %>% #'Multiracial...' in detail/dichot and 'Multiple Races' in single
  #       summarise_at(vars(contains(c("median"))), sum) %>%
  #       mutate(RACE = "Total Multirace")) %>% 
  #   bind_rows(
  #     detail %>% 
  #       filter(grepl(paste(c('Multi','Two'), collapse='|'), RACE)) %>%  #Include 'Two or More Races'
  #       summarise_at(vars(contains(c("median"))), sum) %>% 
  #       mutate(RACE = "Total Multiple Races")) %>% 
  #   bind_rows(
  #     detail %>% 
  #       filter(!grepl(paste(c('White','Total'), collapse='|'), RACE)) %>% #All races, not white and not including the 'Total' rows
  #       summarise_at(vars(contains(c("median"))), sum) %>% 
  #       mutate(RACE = "Total People of Color")
  #   )
  
  # create data year and geography columns
  context <- data.frame(
    data_year = dyear,
    county = geography
  )
  # repeat context df for as many rows as the original data frame
  full_context <- cbind(context, rep(row.names(context)), each=nrow(detail))
  
  # bind the race data with the context information
  df_w_context <- cbind(detail, full_context)

  return(df_w_context)

}

## 2b. This function creates a list of 3 data frames for each of the value types 
## (one for count, one for share, and one for reliability) based on the race_var (PRACE, HRACE, or ARACE) specified in function 2a
# separate_values <- function(df, race_cat=NULL){
#   race_tables <- list()
#   race_tables[[1]] <- df %>% 
#     dplyr::select(RACE, contains("count_"), DATA_YEAR, COUNTY)
#   race_tables[[2]] <- df %>% 
#     dplyr::select(RACE, contains("share"), DATA_YEAR, COUNTY)
#   race_tables[[3]] <- df %>% 
#     dplyr::select(RACE, contains("reliability"), DATA_YEAR, COUNTY) 
#   
#   names(race_tables)[1:3] <- c(paste0(race_cat, "_count"), 
#                                paste0(race_cat, "_share"), 
#                                paste0(race_cat, "_reliability"))
#   return(race_tables)
# }

separate_values <- function(df, race_cat=NULL){
  race_tables <- list()
  race_tables[[1]] <- df %>%
    dplyr::select(RACE, contains("median"), data_year, county)
  # race_tables[[2]] <- df %>%
  #   dplyr::select(RACE, contains("share"), data_year, county,)
  race_tables[[2]] <- df %>%
    dplyr::select(RACE, contains("reliability"), data_year, county)

  names(race_tables)[1:2] <- c(paste0(race_cat, "_median"),
                               # paste0(race_cat, "_share"),
                               paste0(race_cat, "_reliability"))
  return(race_tables)
}

## 2c. This function incorporates functions 2a and 2b to create a list of 3 data frames for each of the value types (one for count, one for share, and one for reliability), with each of the 3 race_var (PRACE, HRACE, or ARACE) categories as columns for the geography specified; it uses the output from function #1a and #1b 
race_cat_by_geography <- function(df, geography, title){
  map(race_vars, ~geography_count(df, .x, geography)) %>% 
    reduce(full_join, by = "RACE")  %>% 
    separate_values(race_cat = paste0(title, "_", geography)) # title of the data frame with geography - will translate to tab names in excel workbook
}

# ## 3. This function creates a list of 3 lists, each with 3 data frames (per estimate type) showing the different values by race_var - resulting in a total of 9 data frames. It uses the output from function #1b. This function incorporates functions #2a-c and iterates through all of the 3 of the multiracial variable category (detail, dichot, single). The geography needs to be specified. 
# all_by_geography <- function(geography, detail_df, dichot_df, single_df){
#   race_eth <- list()
#   race_eth[[1]] <- map(race_vars, ~geography_count(detail_df, .x, geography)) %>% 
#     reduce(full_join, by = "RACE") %>%
#     separate_values(race_cat = "mrdetail")
#   race_eth[[2]] <- map(race_vars, ~geography_count(dichot_df, .x, geography)) %>% 
#     reduce(full_join, by = "RACE") %>% 
#     separate_values(race_cat = "mrdichot")
#   race_eth[[3]] <- map(race_vars, ~geography_count(single_df, .x, geography)) %>% 
#     reduce(full_join, by = "RACE")  %>% 
#     separate_values(race_cat = "mrsingle")
#   return(race_eth)  
# }

# Walk through of each helper function ------------------------------

## 1a. ctyreg_pums_count(): This function creates one long data frame with each of the geographies, 
# for the race_var specified (PRACE, HRACE, or ARACE) with count, count_moe, reliability, share, and share_moe
test1 <- ctyreg_pums_median("ARACE", pums_raw_hh_mrsingle)
test2 <- ctyreg_pums_median("HRACE", pums_raw_hh_mrsingle)
test3 <- ctyreg_pums_median("PRACE", pums_raw_hh_mrsingle)

## 1b. bundle_count(): This function applies function 1a and creates a list of 3 long data tables - one 
# for each of the race_var (PRACE, HRACE, or ARACE) with count, count_moe, reliability, share, and share_moe values; 
# it iterates through the race_var (PRACE, HRACE, or ARACE), without having to specify each race_var separately in 1a
test4 <- bundle_count(race_vars, pums_raw_hh_mrsingle)
test5 <- bundle_count(race_vars, pums_raw_hh_mrdetail)
test6 <- bundle_count(race_vars, pums_raw_hh_mrdichot)

## 2a. geography_count(): This function creates 1 data frame with the count, reliability, and share 
# for the race_var (PRACE, HRACE, or ARACE) specified - it renames fields so that they can be joined 
# once the tables for the other race_var data frames are generated
test7 <- geography_count(test4, "HRACE", "Region")

## 2b. separate_values(): This function creates a list of 3 data frames for each of the value types 
# (one for count, one for share, and one for reliability) based on the race_var (PRACE, HRACE, or ARACE) specified in function 2a 
test8 <- separate_values(test7)

## 2c. race_cat_by_geography(): This function incorporates functions 2a and 2b to create a list of 3 
# data frames for each of the value types (one for count, one for share, and one for reliability), with each of the 3 race_var (PRACE, HRACE, or ARACE) categories as columns for the geography specified; it uses the output from function #1a and #1b 
test9 <- race_cat_by_geography(test4, "Region", "mrsingle")

# # 3. all_by_geography(): This function creates a list of 3 lists, each with 3 data frames (per estimate type) showing the different values by race_var - resulting in a total of 9 data frames. It uses the output from function #1b. This function incorporates functions #2a-c and iterates through all of the 3 of the multiracial variable category (detail, dichot, single). The geography needs to be specified. 
# test10 <- all_by_geography("Region", test4, test5, test6)



# Generate numbers ------------------------------
# function #1b (#1a is included in #1b)
# This function creates a list of 3 long data tables - one for each of the race_var (PRACE, HRACE, or ARACE) with count, count_moe, reliability, share, and share_moe values; it iterates through the race_var (PRACE, HRACE, or ARACE) - need to specify multiracial variable category (detail, dichot, single)
rs_mrdetail <- bundle_count(race_vars, pums_raw_hh_mrdetail)
rs_mrdichot <- bundle_count(race_vars, pums_raw_hh_mrdichot)
rs_mrsingle <- bundle_count(race_vars, pums_raw_hh_mrsingle)

# function #2c (#2a and 2b are included in #2c)
# This function creates a list of 3 data frames for each of the value types (one for count, one for share, and one for reliability), with each of the 3 race_var (PRACE, HRACE, or ARACE) categories as columns for the geography specified; it uses the output from function #1b - need to specify geography and multiracial variable category (detail, dichot, single)
mrdetail_region <- race_cat_by_geography(rs_mrdetail, "Region", "mrdetail") #14 rows in each df
mrdichot_region <- race_cat_by_geography(rs_mrdichot, "Region", "mrdichot") #12 rows in each df
mrsingle_region <- race_cat_by_geography(rs_mrsingle, "Region", "mrsingle") #11 rows in each df

mrdetail_king <- race_cat_by_geography(rs_mrdetail, "King", "mrdetail")
mrdichot_king <- race_cat_by_geography(rs_mrdichot, "King", "mrdichot") 
mrsingle_king <- race_cat_by_geography(rs_mrsingle, "King", "mrsingle") 

mrdetail_kitsap <- race_cat_by_geography(rs_mrdetail, "Kitsap", "mrdetail")
mrdichot_kitsap <- race_cat_by_geography(rs_mrdichot, "Kitsap", "mrdichot") 
mrsingle_kitsap <- race_cat_by_geography(rs_mrsingle, "Kitsap", "mrsingle") 

mrdetail_pierce <- race_cat_by_geography(rs_mrdetail, "Pierce", "mrdetail") 
mrdichot_pierce <- race_cat_by_geography(rs_mrdichot, "Pierce", "mrdichot") 
mrsingle_pierce <- race_cat_by_geography(rs_mrsingle, "Pierce", "mrsingle") 

mrdetail_snoho <- race_cat_by_geography(rs_mrdetail, "Snohomish", "mrdetail")
mrdichot_snoho <- race_cat_by_geography(rs_mrdichot, "Snohomish", "mrdichot") 
mrsingle_snoho <- race_cat_by_geography(rs_mrsingle, "Snohomish", "mrsingle") 

# # function 3
# Region <- all_by_geography("Region", rs_mrdetail, rs_mrdichot, rs_mrsingle)
# King <- all_by_geography("King", rs_mrdetail, rs_mrdichot, rs_mrsingle)
# Kitsap <- all_by_geography("Kitsap", rs_mrdetail, rs_mrdichot, rs_mrsingle)
# Pierce <- all_by_geography("Pierce", rs_mrdetail, rs_mrdichot, rs_mrsingle)
# Snohomish <- all_by_geography("Snohomish", rs_mrdetail, rs_mrdichot, rs_mrsingle)

# Modify to generate three different totals - for median
pums_hh_mrdetail_totals <- pums_raw_hh_mrdetail %>%
  mutate(totals_ARACE_num = case_when(grepl('Multi', ARACE) ~1,
                                  grepl('Two', ARACE) ~2,
                                  grepl('White', ARACE) ~3,
                                  grepl('Total', ARACE)~4,
                                  TRUE~NA),
         totals_HRACE_num = case_when(grepl('Multi', HRACE) ~1,
                                  grepl('Two', HRACE) ~2,
                                  grepl('White', HRACE) ~3,
                                  grepl('Total', HRACE)~4,
                                  TRUE~NA),
         totals_PRACE_num = case_when(grepl('Multi', PRACE) ~1,
                                  grepl('Two', PRACE) ~2,
                                  grepl('White', PRACE) ~3,
                                  grepl('Total', PRACE)~4,
                                  TRUE~NA),
         # totals_ARACE = case_when(totals_ARACE_num==1 ~ "Total multirace",
         #                          totals_ARACE_num!=3 | totals_ARACE_num!=4 ~ "Total POC"),
         # totals_HRACE = case_when(),
         # totals_PRACE = case_when(),
         totals_multirace = case_when(totals_ARACE_num==1 | 
                                        totals_HRACE_num==1 |
                                        totals_PRACE_num==1 ~ "Total multirace",
                                      TRUE~NA),
         totals_multiple_races = case_when(totals_ARACE_num==1 | totals_ARACE_num==2 |
                                             totals_HRACE_num==1 | totals_HRACE_num==2 |
                                             totals_PRACE_num==1 | totals_HRACE_num==2~ "Total multiple races",
                                           TRUE~NA),
         totals_poc = case_when(totals_ARACE_num!=3 | totals_ARACE_num!=4 |
                                  totals_HRACE_num!=3 | totals_HRACE_num!=4 |
                                  totals_PRACE_num!=3 | totals_HRACE_num!=4~ "Total POC",
                                TRUE~NA))


group_vars_totals <- c("totals_multirace", "totals_multiple_races", "totals_poc")
# group_vars_totals <- c(race_vars, "totals_mutlirace", "totals_multiple_races", "totals_poc")


ctyreg_pums_median_totals <- function(group_vars_totals, pums_data){
  rs      <- list()
  rs[[1]] <- psrc_pums_median(pums_data, 
                              stat_var = "HINCP",
                              group_vars=c(group_vars_totals), 
                              incl_na=FALSE, 
                              rr=TRUE)
  rs[[2]] <- psrc_pums_median(pums_data, 
                              stat_var = "HINCP",
                              group_vars=c("COUNTY", group_vars_totals), 
                              incl_na=FALSE, 
                              rr=TRUE) %>%
    filter(COUNTY!="Region")
  rs %<>% data.table::rbindlist() %>%
    arrange(DATA_YEAR, COUNTY)
  return(rs)
}


test1 <- ctyreg_pums_median_totals(c("ARACE", "totals_multirace"), pums_hh_mrdetail_totals) #%>%
  # filter(totals_mutlirace!="Total")
calculating_totals_trial_multirace_arace <-psrc_pums_median(pums_hh_mrdetail_totals,
                                                            stat_var = "HINCP",
                                                            group_vars=c("ARACE","totals_multirace"),
                                                            incl_na=FALSE,
                                                            rr=TRUE)
calculating_totals_trial_multirace_arace_cnt <-psrc_pums_median(pums_hh_mrdetail_totals,
                                                            stat_var = "HINCP",
                                                            group_vars=c("COUNTY","ARACE","totals_multirace"),
                                                            incl_na=FALSE,
                                                            rr=TRUE)
calculating_totals_trial_multirace_arace_combined <- rbind(calculating_totals_trial_multirace_arace, 
                                                           calculating_totals_trial_multirace_arace_cnt) %>% 
  filter(totals_multirace != "Total",
         ARACE != "Total")

calculating_totals_trial_multiple_arace <-psrc_pums_median(pums_hh_mrdetail_totals,
                                                           stat_var = "HINCP",
                                                           group_vars=c("ARACE","totals_multiple_races"),
                                                           incl_na=FALSE,
                                                           rr=TRUE)
calculating_totals_trial_poc_arace <-psrc_pums_median(pums_hh_mrdetail_totals,
                                                      stat_var = "HINCP",
                                                      group_vars=c("ARACE","totals_poc"),
                                                      incl_na=FALSE,
                                                      rr=TRUE)

bundle_count_trial <- purrr::partial(sapply, FUN=ctyreg_pums_median, simplify=FALSE, USE.NAMES=TRUE)

bundle_totals_detail_trial <- bundle_count_trial(c(race_vars, group_vars_totals), pums_hh_mrdetail_totals)

geography_count_totals <- function(df, race_vars, geography){
  detail <- df[[race_vars]] %>%
    filter(COUNTY==geography) %>%
    dplyr::select(race_vars, HINCP_median, reliability) %>%
    rename_with(.cols=HINCP_median:reliability, function(x){paste0(x, "_", race_vars)}) %>%
    rename("RACE"=race_vars) %>% 
    filter(RACE != "Total")

  # create data year and geography columns
  context <- data.frame(
    data_year = dyear,
    county = geography
  )
  # repeat context df for as many rows as the original data frame
  # full_context <- cbind(context, rep(row.names(context)), each=nrow(detail))
  
  # bind the race data with the context information
  df_w_context <- cbind(detail, context)
  
  return(df_w_context)
  
}

test7 <- geography_count_totals(bundle_totals_detail_trial, "totals_mutlirace", "Region")
test8 <- geography_count_totals(bundle_totals_detail_trial, "totals_mutliple_races", "Region")
test9 <- geography_count_totals(bundle_totals_detail_trial, "totals_poc", "Region")


# separate_values_totals <- function(df, group_vars_totals=NULL){
#   race_tables <- list()
#   race_tables[[1]] <- df %>%
#     dplyr::select(RACE, contains("median"), data_year, county)
#   race_tables[[2]] <- df %>%
#     dplyr::select(RACE, contains("reliability"), data_year, county)
# 
#   # names(race_tables)[1:2] <- c(paste0(group_vars_totals, "_median"),
#   #                              # paste0(group_vars_totals, "_share"),
#   #                              paste0(group_vars_totals, "_reliability"))
#   return(race_tables)
# }

test8 <- separate_values(test7)

race_cat_by_geography_totals <- function(df, geography, title){
  map(group_vars_totals, ~geography_count_totals(df, .x, geography)) %>% 
    reduce(full_join, by = c("RACE", "HINCP_median", "reliability", "data_year", "county"))  #%>%
    # separate_values(group_vars_totals = paste0(title, "_", geography)) # title of the data frame with geography - will translate to tab names in excel workbook
}
  
# mrdetail_region <- race_cat_by_geography(rs_mrdetail, "Region", "mrdetail") #14 rows in each df
mrdetail_region_trial <- race_cat_by_geography_totals(bundle_totals_trial, "Region", "mrdetail") #14 rows in each df


# # Export to workbook -------------------------------
# list_df <- c(mrdetail_region, mrdichot_region, mrsingle_region,
#              mrdetail_king, mrdichot_king, mrsingle_king,
#              mrdetail_kitsap, mrdichot_kitsap, mrsingle_kitsap,
#              mrdetail_pierce, mrdichot_pierce, mrsingle_pierce,
#              mrdetail_snoho, mrdichot_snoho, mrsingle_snoho)
# list_df_single <- c(mrsingle_region,
#                     mrsingle_king,
#                     mrsingle_kitsap,
#                     mrsingle_pierce,
#                     mrsingle_snoho)
# 
# excel_data <- write.xlsx(list_df, file.path(output_loc, "hh_re_medincome_numbers.xlsx"))
# excel_data_single <- write.xlsx(list_df_single, file.path(output_loc, "hh_re_medincome_numbers_single.xlsx"))
# 
# # look at workbook
# # sheets_names <- names(excel_data)
