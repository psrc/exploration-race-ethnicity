# This is a prep script
# You will need to do this initial step to install both branches so that you can retrieve each PUMS data object 
# (since the correct race/ethnicity variable categories are distinct). It will create 2 folders 
# within the R directory (library_loc) to save the corresponding data: "psrccensus_mrdetail" and "psrccensus_mrdichot"

library(remotes)
library(purrr)

# library_loc <- "C:/Users/CLam/AppData/Local/R/win-library/4.4"
library_loc <- "C:/Users/eclute/AppData/Local/R/win-library/4.4"

paths <- file.path(library_loc, c("psrccensus_mrdetail", "psrccensus_mrdichot", "psrccensus"))
walk(paths, ~dir.create(.x))

install_github("psrc/psrccensus", 
               ref = "multiracial_detail",
               lib = paths[1],
               force = TRUE)

install_github("psrc/psrccensus", 
               ref = "multiracial_dichotomy",
               lib = paths[2],
               force = TRUE)

install_github("psrc/psrccensus", 
               lib = paths[3],
               force = TRUE)

