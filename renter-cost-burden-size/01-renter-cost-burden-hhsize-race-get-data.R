library(tidyverse)

race_vars <- c("PRACE", "ARACE", "HRACE")
dyear <- 2023

# Go up three directory levels
user_dir <- dirname(dirname(dirname(here())))

library_loc <- file.path(user_dir, "AppData/Local/R/win-library/4.4")
# library_loc <- "C:/Users/eclute/AppData/Local/R/win-library/4.4"
# library_loc <- "C:/Users/mrichards/AppData/Local/R/win-library/4.4"
dir <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

# Retrieve data ----
library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdetail"))
pums_raw_hh_mrdetail <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "NP", "GRPIP", "OWN_RENT"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdichot"))
pums_raw_hh_mrdichot <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "NP", "GRPIP", "OWN_RENT"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus)
pums_raw_hh_mrsingle <- get_psrc_pums(5, dyear , "h", 
                                      c(race_vars, "NP", "GRPIP", "OWN_RENT"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing downloads


# modify for hhsize and rent burden ----
# GRPIP = Gross rent as a percentage of household income past 12 months
pums_raw_hh_mrdetail <- pums_raw_hh_mrdetail %>%
  mutate(hhsz_binary = case_when(NP==1~"single-person",
                                 NP>1~"multi-person"),
         rent_burden = case_when(OWN_RENT =="Rented" & GRPIP<30  ~"not cost-burdened",
                                 OWN_RENT =="Rented" & GRPIP>=30 ~"cost-burdened",
                                 TRUE~NA))  # This NA, paired with the incl_na=FALSE below, keeps fractions to renters reporting GRPIP only


pums_raw_hh_mrdichot <- pums_raw_hh_mrdichot %>%
  mutate(hhsz_binary = case_when(NP==1~"single-person",
                                 NP>1~"multi-person"),
         rent_burden = case_when(OWN_RENT =="Rented" & GRPIP<30  ~"not cost-burdened",
                                 OWN_RENT =="Rented" & GRPIP>=30 ~"cost-burdened",
                                 TRUE~NA))  # This NA, paired with the incl_na=FALSE below, keeps fractions to renters reporting GRPIP only


pums_raw_hh_mrsingle <- pums_raw_hh_mrsingle %>%
  mutate(hhsz_binary = case_when(NP==1~"single-person",
                                 NP>1~"multi-person"),
         rent_burden = case_when(OWN_RENT =="Rented" & GRPIP<30  ~"not cost-burdened",
                                 OWN_RENT =="Rented" & GRPIP>=30 ~"cost-burdened",
                                 TRUE~NA))  # This NA, paired with the incl_na=FALSE below, keeps fractions to renters reporting GRPIP only
