# This script contains the definition of function "create_plots()". Call the function in plots.Rmd or other scripts.
# Use the following argument options:
# Args:
# indicator: "household-count", "ownership_rate", "renter-cost-burden", or "median-income"
# vars_options: 1,2, or 3, where 1 = by re, 2 = by re and hhsize, 3 = by re, hhsize, and tenure
# ind_value:  "share", "count", or "median"

library(tidyverse)
library(openxlsx)
library(scales)
library(showtext)
library(psrcplot)
library(here)

source(here::here('visuals/function-facets.R'))

create_plots <- function(indicator, vars_options = c(1, 2, 3), ind_value = c("share", "count", "median")) {
  
  if(indicator == "median-income" & ind_value != "median") {
    message("The indicator output value for 'median-income' must be 'median'")
    return(NULL) # Exit the function and return NULL
  }
  
  vars <- c("-by-re.xlsx", "-by-re-hhsize.xlsx", "-by-re-hhsize-tenure.xlsx")
  
  e <- data.frame(first = c("detail", "dichot"), second = c("mp", "sp"), third = c("own", "rent"))
  
  df_e <- expand.grid(e) |> 
    mutate(vo2 = paste(first, second, sep = "_")) |> 
    mutate(vo3 = paste(first, second, third, sep = "_"))
  
  all_plots <- list()
  all_tables <- list()
  
  datafile <- paste0(indicator, vars[[vars_options]])
  
  if(vars_options == 1) {
    tab_names <- unique(df_e$first)
  } else if(vars_options == 2) {
    tab_names <- unique(df_e$vo2)
  } else if(vars_options == 3) {
    tab_names <- unique(df_e$vo3)
  }
  
  for(t in tab_names) {
    
    if(t == "detail") {
      re_order <- c(11,15,18,16,13,1,2,3,4,9,10,19,17,5,6,7,8,14,12)
      gtot <- re_order[1:5]
      mtot <- tail(re_order, 6)
    } else if (t == "dichot") {
      re_order <- c(9, 13, 16, 14, 11, 1, 2, 3, 4, 7, 8, 17, 15, 5, 6, 12, 10)
      gtot <- re_order[1:5]
      mtot <- tail(re_order, 4)
    } else if (t == "single") {
      re_order <- c(8,12,15,13,10,1,2,3,4,6,7,16,14,5,11, 9)
      
    }
    
    df <- read.xlsx(here::here("visuals", datafile), sheet = t)
    
    r <- df |> 
      filter(COUNTY == "Region") |> 
      select(ID, RACE, ends_with(c("share", "count"))) 
    
    r <- df |> 
      filter(COUNTY == "Region") |> 
      select(ID, RACE, ends_with(ind_value), ends_with(paste0(ind_value, "_moe"))) 
    
    races_ord <- paste0(r$ID, "_", r$RACE)
    
    if(t == "dichot") browser()
    
    df_long <- r |>
      mutate(race_id = paste0(ID, "_", RACE)) |> 
      pivot_longer(cols = setdiff(colnames(r),c("ID", "RACE", "race_id")),
                   names_to = "variable",
                   values_to = "value"
      ) |> 
      mutate(
        # Create median/moe indicator
        type = if_else(str_detect(variable, "_moe$"), "moe", ind_value),
        
        # Clean variable to just the description
        description = str_remove(variable, "_moe$"),
        description = str_trim(str_replace(description, paste0("_", ind_value), "")),
        description = str_replace(description, "_?HINCP_?", "")
      ) |>
      select(race_id, ID, RACE, description, type, value) |>
      pivot_wider(names_from = type,
                  values_from = value
      ) |>
      mutate(upper = !!sym(ind_value) + moe,
             lower = !!sym(ind_value) - moe) |>
      mutate(description = factor(description, levels = c("PRACE", "ARACE", "HRACE")),
             RACE2 = factor(race_id, levels = races_ord)) |>
      arrange(RACE2, description) |> 
      mutate(facets = case_when(ID %in% gtot ~ "Grand Totals",
                                ID %in% setdiff(unique(r$ID), c(gtot, mtot)) ~ "Alone",
                                ID %in% mtot ~ "Multirace")) |> 
      mutate(facets = factor(facets, levels = c("Grand Totals", "Alone", "Multirace")),
             RACE = factor(RACE, levels = unique(.data[['RACE']])))
    
    all_tables[[t]] <- df_long

    plot_name <- str_replace_all(vars[vars_options], "-", " ") |> 
      str_replace_all(".xlsx", "") |> 
      str_replace_all("\\sre", " Race and Ethnicity") |> 
      str_replace_all("hhsize", "Household Size")
    
    plot_name <- str_replace_all(plot_name, "Ethnicity ", "Ethnicity, " ) |> 
      str_replace_all(" tenure", ", and Tenure")

    subtitle_name <-  str_replace_all(t, "_", " ") |> 
      str_to_title() |> 
      str_replace_all("Mp", "Multi-Person") |>   
      str_replace_all("Sp", "Single-Person")
    
    ind <- str_replace_all(indicator, "-", " ") |> 
      str_to_title()
    
    sub <- str_squish(paste0(subtitle_name, plot_name))
    all_plots[[t]] <- create_facet_chart(df = df_long, title = ind, subtitle = sub, x_val = "RACE")
  }
  
 return(list(tables = all_tables, plots = all_plots))
}


# test <- create_plots("household-count", 1, "count")
# test2 <- create_plots("household-count", 2, "share")
# test3 <- create_plots("median-income", 3, "median")
