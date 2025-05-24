# This script combines the two RDS files and separates the data frames by geography, median values, and reliability into 
# an excel workbook

library(tidyverse)
library(openxlsx)

race_vars <- c("ARACE", "PRACE", "HRACE")
table_types <- c("detail", "dichot", "single")

file_names <- c("non-total-count-df-singleperson.rds", "total-count-df-singleperson.rds")
#file_names <- c("non-total-count-df-multiperson.rds", "total-count-df-multiperson.rds")

# compile into one df ----

df_bind <- map(file_names, ~readRDS(file.path("ownership-rate-hhsize/data/", .x))) |>
  bind_rows() |>
  rename_with(toupper, c(race, table_type))

dfs_share <- df_bind |> 
  select(!c(reliability, ends_with("moe")))

dfs_rel <- df_bind |> 
  select(!starts_with("H"))

# separate into different dataframes ----
all_dfs <- list()

# Loop through the list of dataframes and add them as sheets ----

for (ttype in table_types) {
    id_cols <- c("DATA_YEAR", "COUNTY", "RACE", "TABLE_TYPE")
    
    df_rel <- dfs_rel |>
      filter(TABLE_TYPE == ttype)|>
      pivot_wider(id_cols = id_cols,
                  names_from = "race_type",
                  names_glue = "{race_type}_{.value}",
                  values_from = c("count", "share", "reliability"))|>
      arrange(COUNTY)

    all_dfs[[paste(ttype, sep = "_")]] <- df_rel
}

write.xlsx(all_dfs, "ownership-rate-hhsize/data/ownership_rate-by-re-singlepersonhh.xlsx")
#write.xlsx(all_dfs, "ownership-rate-hhsize/data/ownership_rate-by-re-multipersonhh.xlsx")

