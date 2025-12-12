# This script combines the two RDS files and separates the data frames by geography, median values, and reliability into 
# an excel workbook

library(tidyverse)
library(openxlsx)

race_vars <- c("PRACE", "ARACE", "HRACE")
table_types <- c("detail", "dichot", "single")

file_names <- c("non-total-counts-df.rds","total-counts-df.rds")

# compile into one df ----

df_bind <- map(file_names, ~readRDS(file.path("median-income-hhsize/data/", .x))) |>
  bind_rows() |>
  rename_with(toupper, c(race, table_type)) |>
  rename("HHSIZE" = "hhsz_binary")

dfs_med <- df_bind |> 
  select(!c(reliability, ends_with("moe")))

dfs_rel <- df_bind |> 
  select(!starts_with("HIN"))

# separate into different dataframes ----

geographies <- unique(df_bind$COUNTY)
hhsizes <- c("single-person", "multi-person")
all_dfs <- list()

# Loop through the list of dataframes and add them as sheets ----

for (ttype in table_types) {
  for(g in geographies) {
    for(s in hhsizes) {
      id_cols <- c("DATA_YEAR", "COUNTY", "RACE", "TABLE_TYPE")
      
      df_rel <- df_bind |>
        filter(TABLE_TYPE == ttype & HHSIZE == s)|>
        pivot_wider(id_cols = id_cols,
                    names_from = "race_type",
                    names_glue = "{race_type}_{.value}",
                    values_from = c("HINCP_median", "HINCP_median_moe", "reliability"))|>
        mutate(HH_SIZE = s) |>
        arrange(COUNTY)
      
      s_abbr <- switch(s, "single-person" = "sp", "multi-person" = "mp")
      all_dfs[[paste(ttype, s_abbr, sep = "_")]] <- df_rel
    }}
}

write.xlsx(all_dfs, "median-income-hhsize/data/median-income-by-re-hhsize.xlsx")
