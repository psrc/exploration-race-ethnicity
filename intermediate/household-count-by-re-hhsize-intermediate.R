# This script is meant to transform the .xlsx outputs generated from the indicator 00-04.R files (saved within each indicator/sub-indicator folder within GitHub\exploration-race-ethnicity), so that they can be visualized with the plots.Rmd (saved in GitHub\exploration-race-ethnicity\visuals). This will hopefully prepare the data so that the data gathering/preparation and visualization scripts don't need to be adjusted - we will be visualizing some rows multiple times and do not want to alter any of the calculations. 

# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(here)

# Variables
excel_file_path <- 'household-size/data' #folder path
indicator_file <- 'household-count-by-re-hhsize.xlsx' # Excel file name

# Order for visuals ----
county_order <- c('Region', 'King', 'Kitsap', 'Pierce', 'Snohomish')
race_order_detail <- c(11, #'Total',
                       15, #'Total People of Color',
                       18, #'White alone',
                       16, #'Total Single-race',
                       13, #'Total Multirace',
                       1, #'American Indian or Alaskan Native alone',
                       2, #'Asian alone',
                       3, #'Black or African American alone',
                       4, #'Hispanic or Latino',
                       9, #'Native Hawaiian and Other Pacific Islander alone',
                       10, #'Some Other Race alone',
                       19, #'White alone',
                       17, #'Two or More Races',
                       5, #'Multiracial incl. Asian',
                       6, #'Multiracial incl. Asian, white', 
                       7, #'Multiracial incl. white',
                       8, #'Multiracial not Asian or white',
                       14 #'Total Multirace'
) 
race_order_dichot <- c(9, #'Total',
                       13, #'Total People of Color',
                       16, #'White alone',
                       14, #'Total Single-race',
                       11, #'Total Multirace',
                       1, #'American Indian or Alaskan Native alone',
                       2, #'Asian alone',
                       3, #'Black or African American alone',
                       4, #'Hispanic or Latino',
                       7, #'Native Hawaiian and Other Pacific Islander alone',
                       8, #'Some Other Race alone',
                       17, #'White alone',
                       15, #'Two or More Races',
                       5, #'Multiracial incl. white',
                       6, #'Multiracial not white', 
                       12 #'Total Multirace'
)
race_order_single <- c(8, #'Total',
                       12, #'Total People of Color',
                       15, #'White alone',
                       13, #'Total Single-race',
                       10, #'Total Multirace',
                       1, #'American Indian or Alaskan Native alone',
                       2, #'Asian alone',
                       3, #'Black or African American alone',
                       4, #'Hispanic or Latino',
                       6, #'Native Hawaiian and Other Pacific Islander alone',
                       7, #'Some Other Race alone',
                       16, #'White alone',
                       14, #'Two or More Races',
                       5, #'Multiple Races',
                       11 #'Total Multirace'
)

# ---- CONFIGURATION #1 ---- for organizing by specified order

# Path to your Excel file
# file_path <- "C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/visuals/household-count-by-re.xlsx"
file_path <- file.path(here(), excel_file_path, indicator_file)

# Path to output location
# output_loc <- "C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/intermediate"
output_path <- file.path(here(),'intermediate')


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
  
  # Organize data based on desired order - depends on the TABLE_TYPE (detail, dichot, single)
  if(any(df_all$TABLE_TYPE == "detail")) {
    df_final <- df_all %>%
      arrange(match(COUNTY, county_order),
              match(ID, race_order_detail))
  } else if(any(df_all$TABLE_TYPE == "dichot")) {
    df_final <- df_all %>%
      arrange(match(COUNTY, county_order),
              match(ID, race_order_dichot))
  } else if(any(df_all$TABLE_TYPE == "single")) {
    df_final <- df_all %>%
      arrange(match(COUNTY, county_order),
              match(ID, race_order_single))
  }
  
  # return(df_final)
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


# ---- CONFIGURATION #2 ---- no order required because dealt with in visualization script
# Variables
excel_file_path <- 'household-size/data' #folder path
indicator_file <- 'household-count-by-re-hhsize.xlsx' # Excel file name

# Path to your Excel file
# file_path <- "C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/visuals/household-count-by-re.xlsx"
file_path <- file.path(here(), excel_file_path, indicator_file)

# Path to output location
# output_loc <- "C:/Users/mrichards/Documents/GitHub/exploration-race-ethnicity/intermediate"
output_path <- file.path(here(),'intermediate/test')



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
  
  # if(any(df_all$HH_SIZE != "single-person")){
  #   df_multirace <- df %>%   
  #     filter(RACE=="Total Multirace") %>% #replicate row with total multirace 
  #     rbind(df_white)
  #   
  #   # Create unique ID
  #   df_all <- df_multirace %>%
  #     arrange(match(COUNTY, county_order),
  #             RACE) %>% 
  #     group_by(COUNTY) %>%
  #     dplyr::mutate(ID = row_number())
  # } else if(any(df_all$HH_SIZE == "single-person")) {
  #   # Create unique ID
  #   df_all <- df_white %>%
  #     arrange(match(COUNTY, county_order),
  #             RACE) %>% 
  #     group_by(COUNTY) %>%
  #     dplyr::mutate(ID = row_number())
  # }
  
  # Rename Total row for visuals
  df_all$RACE[df_all$RACE=='Total'] <- 'Grand Total'
  
  # Organize data based on county - for ease of checking
  df_final <- df_all %>% 
    arrange(match(COUNTY, county_order))
  
  # # Organize data based on desired order - depends on the TABLE_TYPE (detail, dichot, single)
  # if(any(df_all$HH_SIZE == "single-person")) {
  #   df_final <- df_all %>%
  #     arrange(match(COUNTY, county_order))
  #             # match(ID, race_order_sp))
  # } else if(any(df_all$TABLE_TYPE == "detail")) {
  #   df_final <- df_all %>%
  #     arrange(match(COUNTY, county_order))
  #             # match(ID, race_order_detail_mp))
  # } else if(any(df_all$TABLE_TYPE == "dichot")) {
  #   df_final <- df_all %>%
  #     arrange(match(COUNTY, county_order))
  #             # match(ID, race_order_dichot_mp))
  # } else if(any(df_all$TABLE_TYPE == "single")) {
  #   df_final <- df_all %>%
  #     arrange(match(COUNTY, county_order))
  #             # match(ID, race_order_single_mp))
  # }
  
  # return(df_final)
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

