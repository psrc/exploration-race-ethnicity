# This script combines the two RDS files and separates the data frames by geography, median values, and reliability into 
# an excel workbook

library(tidyverse)
library(openxlsx)

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

file_names <- c("non-total-counts-df.rds","total-counts-df.rds")

# compile into one df ----

df_bind <- map(file_names, ~readRDS(file.path("renter-cost-burden/data/", .x))) |> 
  bind_rows() |> 
  rename_with(toupper, c(race, table_type))

dfs_count <- df_bind |> 
  select(!c(reliability, ends_with("moe")))

dfs_rel <- df_bind |> 
  select(!starts_with("H"))

# separate into different dataframes ----

geographies <- unique(df_bind$COUNTY)
hhsizes <- c("single-person", "multi-person")
all_dfs <- list()

# Loop through the list of dataframes and add them as sheets ----

for (ttype in table_types) {
  for(g in geographies) {
    id_cols <- c("DATA_YEAR", "COUNTY", "RACE", "TABLE_TYPE")
    
    df_rel <- df_bind |>
      filter(TABLE_TYPE == ttype)|>
      pivot_wider(id_cols = id_cols,
                  names_from = "race_type",
                  names_glue = "{race_type}_{.value}",
                  values_from = c("count", "share", "reliability"))|>
      arrange(COUNTY)
    
    all_dfs[[paste(ttype, sep = "_")]] <- df_rel
    
   
  }
}


write.xlsx(all_dfs, "renter-cost-burden/data/renter-cost-burden-by-re.xlsx")
