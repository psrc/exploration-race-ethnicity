# This script is meant to transform the .xlsx outputs generated from the indicator 00-04.R files (saved within each indicator/sub-indicator folder within GitHub\exploration-race-ethnicity), so that they can be visualized with the plots.Rmd (saved in GitHub\exploration-race-ethnicity\visuals). This will hopefully prepare the data so that the data gathering/preparation and visualization scripts don't need to be adjusted - we will be visualizing some rows multiple times and do not want to alter any of the calculations. 

# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(here)

# Variables
excel_file_path <- 'ownership-rate/data' #folder path
indicator_file <- 'ownership-rate-by-re.xlsx' # Excel file name

# Order for visuals ----
county_order <- c('Region', 'King', 'Kitsap', 'Pierce', 'Snohomish')

# ---- CONFIGURATION ----

# Path to your Excel file
file_path <- file.path(here(), excel_file_path, indicator_file)

# Path to output location
output_path <- file.path(here(),'visuals')

# Processing function
organize_data <- function(df) {
  
  # Replicate rows for visualization
  df_white <- df %>% 
    filter(RACE == "White alone") %>% #replicate row with white 
    rbind(df) #bind to original data 
  
  df_multirace <- df %>%   
    filter(RACE=="Total Multirace") %>% #replicate row with total multirace 
    rbind(df_white)
  
  # Create unique ID
  df_all <- df_multirace %>%
    arrange(match(COUNTY, county_order),
            RACE) %>% 
    group_by(COUNTY) %>%
    dplyr::mutate(ID = row_number())
  
  # Rename Total row for visuals
  df_all$RACE[df_all$RACE=='Total'] <- 'Grand Total'
  
  # Organize data based on county - for ease of checking
  df_final <- df_all %>% 
    arrange(match(COUNTY, county_order))

}

try({  
  # Get all sheet names
  sheet_names <- excel_sheets(file_path)
  
  # Read each sheet into a list of data frames
  sheets_list <- lapply(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet)
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