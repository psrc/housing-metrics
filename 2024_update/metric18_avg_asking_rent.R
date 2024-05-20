# Processing Raw CoStar Data for Housing Monitoring
# Geographies: RGCs, HCTs, Counties, Region
# Data Vintage: Q2 2024 QTD
# Created By: Eric Clute

# Assumptions ---------------------
library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)

rgc_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_rgc"
hct_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_hct"
county_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_county"
reference_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/reference_table.xlsx"
export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent"
source_info <- c("CoStar, Q2 2024 QTD. Calculated by Eric Clute, 5/20/2024. Includes all data inside RGC/HCT (excludes Federal Way, Issaquah, Port Orchard Ferry Terminal, and Poulsbo due to missing data). Rent rounded to nearest 10")
region_source_info <- c("CoStar, Q2 2024 QTD. Calculated by Eric Clute, 5/20/2024. Rent rounded to nearest 10")

# Create Functions ---------------------
# Function to read Excel file, extract column names and first row, and add filename as a column
read_excel_with_filename <- function(file_path) {
  # Read Excel file
  data <- read_excel(file_path)
  
  # Extract column names and first row of data
  col_names <- colnames(data)
  first_row <- slice(data, 1)
  
  # Add filename as a column without the .xlsx extension
  filename <- gsub("\\.xlsx$", "", basename(file_path))
  first_row$rgc <- filename
  
  # Convert all columns to character type
  first_row <- mutate_all(first_row, as.character)
  
  # Return a list containing column names, first row of data, and filename
  list(col_names = col_names, first_row = first_row)
}

# Function to combine data from each Excel file into one data frame
combine_data <- function(data_list) {
  result <- combined_data
  for (i in seq_along(data_list)) {
    filename <- gsub("\\.xlsx$", "", basename(xlsx_files[i]))
    data_list[[i]]$first_row$rgc <- filename
    # Convert all columns to character type
    data_list[[i]]$first_row <- mutate_all(data_list[[i]]$first_row, as.character)
    result <- bind_rows(result, data_list[[i]]$first_row)
  }
  result
}

# Clean county data and generate regional numbers (inside and outside of RGCs) --------------------
xlsx_files <- list.files(county_data_path, pattern = "\\.xlsx$", full.names = TRUE) # Get a list of all Excel files in the folder
excel_data <- map(xlsx_files, read_excel_with_filename) # Read all Excel files
all_col_names <- excel_data %>% map("col_names") %>% reduce(union) # Combine all column names into one vector
combined_data <- tibble::tibble(!!!set_names(rep(list(NULL), length(all_col_names)), all_col_names)) # Create an empty data frame with all column names

combined_data <- combine_data(excel_data)

by_fullregion <- combined_data %>%
  select(rgc, `Inventory Units`, `Asking Rent Per Unit`) %>%
  mutate(`Asking Rent Per Unit` = as.numeric(`Asking Rent Per Unit`),
         `Inventory Units` = as.numeric(`Inventory Units`)) %>%
  rename(geography = rgc)

# Calculate aggregate rent collected
by_fullregion <- by_fullregion %>%
  mutate(`Asking Rent Per Unit` = as.numeric(`Asking Rent Per Unit`),
         `Inventory Units` = as.numeric(`Inventory Units`))

by_fullregion$agg_askingrent <- (by_fullregion$`Asking Rent Per Unit` * by_fullregion$`Inventory Units`)

# Calculate average rent
by_fullregion <- by_fullregion %>%
  summarise(
    geography = "region",
    `Inventory Units` = sum(`Inventory Units`),
    `agg_askingrent` = sum(`agg_askingrent`)
  ) %>%
  select(geography, `Inventory Units`, `agg_askingrent`)

by_fullregion$avg_askingrent = by_fullregion$agg_askingrent / by_fullregion$`Inventory Units`

by_fullregion <- by_fullregion %>%
  select(-agg_askingrent)

# Combine Individual Center's Data Together  ---------------------
xlsx_files <- list.files(rgc_data_path, pattern = "\\.xlsx$", full.names = TRUE) # Get a list of all Excel files in the folder
excel_data <- map(xlsx_files, read_excel_with_filename) # Read all Excel files
all_col_names <- excel_data %>% map("col_names") %>% reduce(union) # Combine all column names into one vector
combined_data <- tibble::tibble(!!!set_names(rep(list(NULL), length(all_col_names)), all_col_names)) # Create an empty data frame with all column names

# Call the function to combine data from all Excel files
combined_data <- combine_data(excel_data)

# Formatting & Calculating New Field ---------------------

# Remove unneeded fields
combined_data <- combined_data %>%
  select(rgc, `Inventory Units`, `Asking Rent Per Unit`)

# Combine w/reference table
reference_table <- read_excel(reference_path)
combined_data <- left_join(combined_data, reference_table, by = "rgc")

# Create aggregate rent field for final tabulation of avg rents across centers
combined_data <- combined_data %>%
  mutate(`Asking Rent Per Unit` = as.numeric(`Asking Rent Per Unit`),
         `Inventory Units` = as.numeric(`Inventory Units`))

combined_data$agg_askingrent <- (combined_data$`Asking Rent Per Unit` * combined_data$`Inventory Units`)

# Summarize by all centers in region
by_allcenters <- combined_data %>%
  summarize(
    `Inventory Units` = sum(`Inventory Units`, na.rm = TRUE),
    agg_askingrent = sum(agg_askingrent, na.rm = TRUE))

by_allcenters$avg_askingrent = by_allcenters$agg_askingrent / by_allcenters$`Inventory Units`

by_allcenters <- by_allcenters %>%
  select(-agg_askingrent)

by_allcenters$geography <- "all centers"

by_allcenters <- by_allcenters %>%
  select(geography, everything())

# Summarize by County
by_county <- combined_data %>%
  group_by(county) %>%
  summarize(
    `Inventory Units` = sum(`Inventory Units`, na.rm = TRUE),
    agg_askingrent = sum(agg_askingrent, na.rm = TRUE))

by_county$avg_askingrent = by_county$agg_askingrent / by_county$`Inventory Units`

by_county <- by_county %>%
  select(-agg_askingrent)

# Summarize by Center Type
by_centertype <- combined_data %>%
  group_by(center_type) %>%
  summarize(
    `Inventory Units` = sum(`Inventory Units`, na.rm = TRUE),
    agg_askingrent = sum(agg_askingrent, na.rm = TRUE))

by_centertype$avg_askingrent = by_centertype$agg_askingrent / by_centertype$`Inventory Units`

by_centertype <- by_centertype %>%
  select(-agg_askingrent)

# Summarize by Center (name)
by_center <- combined_data %>%
  select(rgc, `Inventory Units`, `Asking Rent Per Unit`, center_type, county)

# Rounding estimates
by_center$`Asking Rent Per Unit` <- round(by_center$`Asking Rent Per Unit` / 10) * 10
by_centertype$avg_askingrent <- round(by_centertype$avg_askingrent / 10) * 10
by_county$avg_askingrent <- round(by_county$avg_askingrent / 10) * 10
by_allcenters$avg_askingrent <- round(by_allcenters$avg_askingrent / 10) * 10
by_fullregion$avg_askingrent <- round(by_fullregion$avg_askingrent / 10) * 10

# Export Data
export_file <- paste0(export_path, "/metric21.xlsx")
work_book <- createWorkbook()

# Add the "by_center" sheet
addWorksheet(work_book, sheetName = "by_center")
writeData(work_book, sheet = "by_center", by_center)
writeData(work_book, sheet = "by_center", x = data.frame(source_info), startRow = nrow(by_center) + 3, startCol = 1)

# Add the "by_centertype" sheet
addWorksheet(work_book, sheetName = "by_centertype")
writeData(work_book, sheet = "by_centertype", by_centertype)
writeData(work_book, sheet = "by_centertype", x = data.frame(source_info), startRow = nrow(by_centertype) + 3, startCol = 1)

# Add the "by_county" sheet
addWorksheet(work_book, sheetName = "by_county")
writeData(work_book, sheet = "by_county", by_county)
writeData(work_book, sheet = "by_county", x = data.frame(source_info), startRow = nrow(by_county) + 3, startCol = 1)

# Add the "by_allcenters" sheet
addWorksheet(work_book, sheetName = "by_allcenters")
writeData(work_book, sheet = "by_allcenters", by_allcenters)
writeData(work_book, sheet = "by_allcenters", x = data.frame(source_info), startRow = nrow(by_allcenters) + 3, startCol = 1)

# Add the "by_fullregion" sheet
addWorksheet(work_book, sheetName = "by_fullregion")
writeData(work_book, sheet = "by_fullregion", by_fullregion)
writeData(work_book, sheet = "by_fullregion", x = data.frame(region_source_info), startRow = nrow(by_fullregion) + 3, startCol = 1)

saveWorkbook(work_book, file = export_file, overwrite = TRUE)
