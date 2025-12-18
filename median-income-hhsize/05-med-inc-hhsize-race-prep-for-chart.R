# This script is meant to transform the .xlsx outputs generated from the indicator 00-04.R files (saved within each indicator/sub-indicator folder within GitHub\exploration-race-ethnicity), so that they can be visualized with the plots.Rmd (saved in GitHub\exploration-race-ethnicity\visuals). This will hopefully prepare the data so that the data gathering/preparation and visualization scripts don't need to be adjusted - we will be visualizing some rows multiple times and do not want to alter any of the calculations. 

# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(here)

# Variables
excel_file_path <- 'median-income-hhsize/data' #folder path
indicator_file <- 'median-income-by-re-hhsize.xlsx' # Excel file name

# Order for visuals ----
county_order <- c('Region', 'King', 'Kitsap', 'Pierce', 'Snohomish')

# ---- CONFIGURATION ----

# Path to your Excel file
file_path <- file.path(here(), excel_file_path, indicator_file)
workbook <- loadWorkbook(file_path)

# Path to output location
output_path <- file.path(here(),'visuals')

# Add empty row for multirace harvard for single-person hh for consistency
# Get all sheet names
sheet_names <- excel_sheets(file_path)

# define target sheet name
target_pattern <- "_sp"

# function to apply to matching sheets
process_sp_sheet <- function(sheet_name){
  message("Processing sheet:", sheet_name)
  
  df <- read_excel(file_path, sheet=sheet_name)
  
  df_multirace_harvard <- df %>% 
    filter(RACE=="Single race Harvard") 
  # rename to Multirace Harvard
  df_multirace_harvard$RACE[df_multirace_harvard$RACE=='Single race Harvard'] <- 'Multirace Harvard' 
  # adjust all numeric fields to 0  
  df_multirace_harvard[, 5:10][df_multirace_harvard[, 5:10] >= 0] <- NA
  # bind with full data set
  df_edit <- df_multirace_harvard %>% 
    rbind(df) %>% 
    arrange(match(COUNTY, county_order),
            RACE) %>% 
    group_by(COUNTY)
  
  return(df_edit)
  
}

# apply function to matching sheets
matching_sheets <- sheet_names[grepl(target_pattern, sheet_names, ignore.case = TRUE)]

sheets_sp <- lapply(matching_sheets, process_sp_sheet)
# sheets <- list(sheets_sp)

# Name the list elements by sheet name
# names(sheets_sp) <- matching_sheets
names(sheets_sp) <- c("detail_sp_0","dichot_sp_0","single_sp_0")

# # Create a new workbook
# wb <- createWorkbook()

for (sheet_name in names(sheets_sp)) {
  # Add a worksheet with the name of the list element
  addWorksheet(workbook, sheet_name)
  
  # Write the data to the worksheet
  writeData(workbook, sheet_name, sheets_sp[[sheet_name]])
}

# ---- Remove old sp sheets ---- tried replacing earlier but didn't work
# Sheets to remove (can be names or numeric indices)
sheets_to_remove <- c("detail_sp","dichot_sp","single_sp")

# Validate and remove sheets safely
for (sheet in sheets_to_remove) {
  if (sheet %in% names(workbook)) {
    removeWorksheet(workbook, sheet)
  } else {
    warning(sprintf("Sheet '%s' not found, skipping.", sheet))
  }
}

# ---- Rename sp sheet ----
names(workbook)[names(workbook) == "detail_sp_0"] <- "detail_sp"
names(workbook)[names(workbook) == "dichot_sp_0"] <- "dichot_sp"
names(workbook)[names(workbook) == "single_sp_0"] <- "single_sp"

saveWorkbook(workbook, 
             file.path(output_path, indicator_file),
             overwrite = TRUE)

# Processing function
organize_data <- function(df) {
  
  # Replicate rows for visualization
  df_total <- df %>% 
    filter(RACE == "Total") %>% #replicate row with total 
    slice(rep(1:n(), each = 3)) %>% 
    rbind(df) #bind to original data
  
  df_white <- df %>% 
    filter(RACE == "White") %>% #replicate row with white 
    rbind(df_total) #bind to original data 
  
  df_multirace <- df %>%   
    filter(RACE=="Multirace PSRC") %>% #replicate row with total multirace psrc
    slice(rep(1:n(), each = 2)) %>% 
    rbind(df_white)
  
  df_singlerace <- df %>%   
    filter(RACE=="Single race PSRC") %>% #replicate row with total single race psrc 
    rbind(df_multirace)
  
  df_twoormore <- df %>%   
    filter(RACE=="Two or More Races") %>% #replicate row with two or more races 
    rbind(df_singlerace)
  
  # Create unique ID
  df_all <- df_twoormore %>%
    arrange(match(COUNTY, county_order),
            RACE) %>% 
    group_by(COUNTY) %>%
    dplyr::mutate(ID = row_number())
  
  # Rename 'MNAW' row for visuals
  df_all$RACE[df_all$RACE=='MNAW'] <- 'Multirace not incl. Asian & white'
  
  # Rename 'MNW' row for visuals
  df_all$RACE[df_all$RACE=='MNW'] <- 'Multirace not incl. white'
  
  # Rename 'Multirace incl. Asian, white' row for visuals
  df_all$RACE[df_all$RACE=='Multirace incl. Asian, white'] <- 'Multirace incl. Asian & white'
  
  # Rename 'Total' row for visuals
  df_all$RACE[df_all$RACE=='Total'] <- 'Region'
  
  # Organize data based on county - for ease of checking
  df_final <- df_all %>% 
    arrange(match(COUNTY, county_order))
  
}

try({  
  # Get all sheet names
  sheet_names <- excel_sheets(file.path(output_path, indicator_file))
  
  # Read each sheet into a list of data frames
  sheets_list <- lapply(sheet_names, function(sheet) {
    read_excel(file.path(output_path, indicator_file), sheet = sheet)
  })
  
  # Name the list elements by sheet name
  names(sheets_list) <- sheet_names
  
  # Apply the same process to each sheet
  processed_sheets <- lapply(sheets_list, organize_data)
  
  # Print sheets
  # print(processed_sheets[[1]])
  
  # Save as excel file for visualization
  writexl::write_xlsx(processed_sheets,
                      path = file.path(output_path, indicator_file))
  message("File saved successfully")
})
