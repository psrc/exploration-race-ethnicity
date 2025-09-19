race_vars <- c("PRACE", "ARACE", "HRACE")
dyear <- 2023
# library_loc <- "C:/Users/CLam/AppData/Local/R/win-library/4.4"
library_loc <- "C:/Users/mrichards/AppData/Local/R/win-library/4.4"
dir <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

# Retrieve data ----
library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdetail"))
pums_raw_hh_mrdetail <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus, lib.loc= file.path(library_loc,"psrccensus_mrdichot"))
pums_raw_hh_mrdichot <- get_psrc_pums(5, dyear, "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing
unloadNamespace("psrccensus")

library(psrccensus)
pums_raw_hh_mrsingle <- get_psrc_pums(5, dyear , "h", 
                                      c(race_vars, "HINCP"), # add any additional variables you may want
                                      dir = dir) # pull network archived copy because Census Bureau ftp site not allowing downloads
