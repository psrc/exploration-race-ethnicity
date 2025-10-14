library(tidyverse)
library(openxlsx)
library(scales)
library(showtext)
library(psrcplot)
library(here)

filenames <- c("household-count", "ownership_rate", "renter-cost-burden", "median-income")

# median-inc tier 3
e <- data.frame(first = c("detail", "dichot"), second = c("mp", "sp"), third = c("own", "rent"))
df_e <- expand.grid(e) |> 
  mutate(tab_name = paste(first, second, third, sep = "_")) |> 
  mutate(second_text = case_when(second == "mp" ~ "Multi-Person",
                                 second == "sp" ~ "Single-Person")) |> 
  mutate(plot_name = str_to_title(paste(first, second_text, third)))

all_plots <- list()

for(t in df_e$tab_name) {
  
  chart_name <- df_e |> filter(tab_name == t) |> pull(plot_name)
  
  df <- read.xlsx(datafile,
                  sheet = t)
  
  r <- df |> 
    filter(COUNTY == "Region") |> 
    select(RACE, ends_with("median"), ends_with("moe")) 
  
  races_ord <- r$RACE
  
  df_long <- r |> 
    pivot_longer(cols = -RACE,
                 names_to = "variable",
                 values_to = "value"
    ) |> 
    mutate(
      # Create median/moe indicator
      type = if_else(str_detect(variable, "_moe$"), "moe", "median"),
      
      # Clean variable to just the description
      description = str_remove(variable, "_moe$"),
      description = str_trim(str_replace(description, "_HINCP_median", ""))
    ) |> 
    select(RACE, description, type, value) |> 
    pivot_wider(names_from = type,
                values_from = value
    ) |> 
    mutate(upper = median + moe,
           lower = median - moe) |> 
    mutate(description = factor(description, levels = c("PRACE", "ARACE", "HRACE")),
           RACE = factor(RACE, levels = races_ord)) |> 
    arrange(RACE, description)
  
  p <- create_bar_chart(df = df_long, title = chart_name)
  all_plots[[t]] <- p
}

all_plots