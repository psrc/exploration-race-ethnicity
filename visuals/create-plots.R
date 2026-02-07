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
library(readxl)

source(here::here('visuals/function-facets.R'))

create_plots <- function(indicator, vars_options = c(1, 2, 3), ind_value = c("share", "count", "median"), 
                         juris = c("Region", "King", "Kitsap", "Pierce", "Snohomish")) {
  
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

  label_lu <- read_excel("T:\\60day-TEMP\\christy\\explore-race\\label-lookup.xlsx", sheet = 1)

  for(t in tab_names) {
    # where to place Harvard and PSRC totals?
    # include single_mp?
    
    if(str_ends(t, "_sp") | str_ends(t, "_sp_.*")) { #detail_sp, dichot_sp, and single_sp
      cols <- c("all_sp", "all_sp_cat")
    } else if (vars_options == 3 & !str_ends(t, "_sp")){
      w <- str_extract(t, ".*(?=_(own|rent))") # remove _own or _rent
      cols <- c(w, paste0(w, "_cat"))
    } else {
      cols <- c(t, paste0(t, "_cat"))
    }
    
    llu <- label_lu |> 
      select(all_of(cols)) |> 
      rename(all_of(setNames(cols, c("order", "facet"))))
    
    df <- read_excel(here::here("visuals", datafile), sheet = t)
    
    r <- df |> 
      filter(COUNTY == juris) |> 
      mutate(order = paste(RACE, ID)) |> 
      left_join(llu, by = "order") |> 
      select(COUNTY, order, facet, ID, RACE, ends_with(ind_value), ends_with(paste0(ind_value, "_moe"))) 
    
    if(indicator == "median-income") {
      facet_levels <- c(juris, "Alone", "Multirace", "Other") ### Edited from "Region"?
    } else {
      facet_levels <- c("Totals", "Alone", "Multirace", "Other") 
    }

    df_long <- r |>
      pivot_longer(cols = setdiff(colnames(r),c("COUNTY", "ID", "RACE", "order", "facet")),
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
      select(COUNTY, order, facet, ID, RACE, description, type, value) |>
      pivot_wider(names_from = type,
                  values_from = value
      ) |>
      mutate(upper = !!sym(ind_value) + moe,
             lower = !!sym(ind_value) - moe) |>
      mutate(description = factor(description, levels = c("PRACE", "ARACE", "HRACE")),
             order = factor(order, levels = llu$order)
             ) |>
      arrange(order, description) |> 
      # mutate(facet = factor(facet, levels = facet_levels)) |> 
      mutate(RACE = factor(RACE, levels = unique(.data[['RACE']])))
    
    # replace 'Region' with juris expression
    if(juris != "Region") {
      df_long <- df_long |>
        mutate(across(c(order, RACE), ~ str_replace_all(.x, "Region", juris))) 
      
      order_vec <- unique(df_long$order)
      df_long <- df_long |> 
        mutate(order = factor(order, levels = order_vec)) |>
        arrange(order, description)
    }
  

    if(indicator == "median-income") {
      df_long <- df_long |> 
        mutate(facet = case_when(facet == "Totals" ~ juris,
                                 .default = facet))
        # mutate(facet = case_when(facet == "Totals" ~ "Region",
        #                          .default = facet))
    }
    
    df_long <- df_long |> 
      mutate(facet = factor(facet, levels = facet_levels)) 
    
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
    all_plots[[t]] <- create_facet_bar_chart(df = df_long, title = ind, subtitle = sub, x_val = "order")
  }
  
 return(list(tables = all_tables, plots = all_plots))
}


# test <- create_plots("household-count", 1, "count", "King")
# test <- create_plots("household-count", 1, "count", "Region")
# test <- create_plots("renter-cost-burden", 1, "count")
# test2 <- create_plots("household-count", 2, "share")
# test1 <- create_plots("median-income", 1, "median")
# test2 <- create_plots("median-income", 2, "median")
# test3 <- create_plots("median-income", 3, "median")
# hhs_re <- create_plots("household-count", 2, "count")
# hhs_re_s <- create_plots("household-count", 1, "share")
# hhs_re_hhsize_s <- create_plots("household-count", 2, "share")
