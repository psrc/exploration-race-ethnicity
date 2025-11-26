# This script combines the two RDS files and separates the data frames by geography, median values, and reliability into 
# an excel workbook

library(tidyverse)
library(openxlsx)
library(scales) #percent()

race_vars <- c("PRACE", "ARACE", "HRACE")
table_types <- c("detail", "dichot", "single")

file_names <- c("non-total-counts-df.rds","total-counts-df.rds")


# compile into one df ----

df_bind <- map(file_names, ~readRDS(file.path("median-income-hhsize-tenure/data/", .x))) |>
  bind_rows() |>
  rename_with(toupper, c(race, table_type)) |>
  rename("HHSIZE" = "hhsz_binary")

# dfs_med <- df_bind |> 
#   select(!c(reliability, ends_with("moe")))

dfs_rel <- df_bind |> 
  select(!starts_with("HIN"))

# separate into different dataframes ----

# geographies <- unique(df_bind$COUNTY)
hhsizes <- c("single-person", "multi-person")
tenure <- c("Owned", "Rented")
all_dfs <- list()

# Loop through the list of dataframes and add them as sheets ----

for (ttype in table_types) {
  # for(g in geographies) {
    for(s in hhsizes) {
      for(t in tenure) {
      id_cols <- c("DATA_YEAR", "COUNTY", "RACE", "TABLE_TYPE")
      
      df_rel <- df_bind |>
        filter(TABLE_TYPE == ttype & HHSIZE == s & OWN_RENT == t)|>
        pivot_wider(id_cols = id_cols,
                    names_from = "race_type",
                    names_glue = "{race_type}_{.value}",
                    values_from = c("HINCP_median", "HINCP_median_moe", "reliability"))|>
        mutate(HH_SIZE = s) |>
        arrange(COUNTY)
      
      s_abbr <- switch(s, "single-person" = "sp", "multi-person" = "mp")
      t_abbr <- switch(t, "Owned" = "own", "Rented" = "rent")
      all_dfs[[paste(ttype, s_abbr, t_abbr, sep = "_")]] <- df_rel
    }}#}
}

write.xlsx(all_dfs, "median-income-hhsize-tenure/data/median-income-by-re-hhsize-tenure.xlsx")



# create tabs with differences ------------------
# join rent/own tables together so that the differences can be calculated

# calc_differences <- function(df1, df2, output_df){
#   
#   output_df <- all_dfs[[df1]] %>% 
#     left_join(all_dfs[[df2]], 
#               by = c('DATA_YEAR', 'COUNTY', 'RACE', 'TABLE_TYPE')) %>% 
#     select(-contains(c("moe","reliability"))) %>% 
#     mutate(dif_PRACE = round(PRACE_HINCP_median.x-PRACE_HINCP_median.y, digits = 2),
#            dif_ARACE = round(ARACE_HINCP_median.x-ARACE_HINCP_median.y, digits = 2),
#            dif_HRACE = round(HRACE_HINCP_median.x-HRACE_HINCP_median.y, digits = 2),
#            percdif_PRACE = percent((PRACE_HINCP_median.x - PRACE_HINCP_median.y) / PRACE_HINCP_median.y, accuracy = 0.1),
#            percdif_ARACE = percent((ARACE_HINCP_median.x - ARACE_HINCP_median.y) / ARACE_HINCP_median.y, accuracy = 0.1),
#            percdif_HRACE = percent((HRACE_HINCP_median.x - HRACE_HINCP_median.y) / HRACE_HINCP_median.y, accuracy = 0.1)) %>% 
#     select(-contains(c(".x", ".y")))
# }
# 
# 
# detail_sp_dif <- calc_differences('detail_sp_own', 'detail_sp_rent')
# detail_mp_dif <- calc_differences('detail_mp_own', 'detail_mp_rent')
# dichot_sp_dif <- calc_differences('dichot_sp_own', 'dichot_sp_rent')
# dichot_mp_dif <- calc_differences('dichot_mp_own', 'dichot_mp_rent')
# single_sp_dif <- calc_differences('single_sp_own', 'single_sp_rent')
# single_mp_dif <- calc_differences('single_mp_own', 'single_mp_rent')
# 
# # create list of df - alphabetical
# dif_list <- lapply(ls(pattern="dif$"), function(x) get(x))
# 
# # Assign names to the data frames - alphabetical to match order of list
# names(dif_list) <- c("detail_mp_dif", "detail_sp_dif",
#                      "dichot_mp_dif", "dichot_sp_dif",
#                      "single_mp_dif", "single_sp_dif")
# 
# # Create temp workbook for differences
# wb <- createWorkbook("median-income-hhsize-tenure/data/differences.xlsx")
#   
# # Append each data frame to a new sheet
# for (name in names(dif_list)) {
#   addWorksheet(wb, name)  # Add a new sheet with the name of the data frame
#   writeData(wb, name, dif_list[[name]])  # Write the data frame to the sheet
# }
# 
# # Save the workbook
# saveWorkbook(wb, 
#              "median-income-hhsize-tenure/data/differences.xlsx", 
#              overwrite = T)
# 
# 
# # COMBINE datasets ------------------
# 
# # List all Excel files in the directory
# file_list <- list.files(path = "median-income-hhsize-tenure/data/", 
#                         pattern = "\\.xlsx$", full.names = TRUE)
# 
# # Create a new workbook
# merged_workbook <- createWorkbook()
# 
# # Loop through each file and add its sheets to the new workbook
# for (file in file_list) {
#   sheets <- getSheetNames(file)
#   for (sheet in sheets) {
#     data <- read.xlsx(file, sheet = sheet)
#     addWorksheet(merged_workbook, sheetName = sheet)
#     writeData(merged_workbook, sheet = sheet, x = data)
#   }
# }
# 
# # Get current sheet names
# sheet_names <- names(merged_workbook)
# 
# # Sort sheet names alphabetically
# sorted_sheet_names <- sort(sheet_names)
# 
# # Reorder sheets based on sorted names
# worksheetOrder(merged_workbook) <- match(sorted_sheet_names, sheet_names)
# 
# # Save the merged workbook
# saveWorkbook(merged_workbook, 
#              file = "median-income-hhsize-tenure/data/median-income-by-re-hhsize-tenure-with-differences.xlsx", 
#              overwrite = TRUE)
