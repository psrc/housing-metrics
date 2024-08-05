# Processing Raw CoStar Data for Housing Monitoring
# Geographies: RGCs, HCTs, Counties
# Data Vintage: Q2 2024 QTD
# Created By: Eric Clute

# Assumptions ---------------------
library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)
library(tidyr)
library(fredr) # Requires API key
library(psrccensus) # Required for inflation adjustment

fredr_set_key("99e2d81f189630d83b9e37ba8ca4f142")

rgc_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_rgc"
hct_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_hct"
county_data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/raw_county"
reference_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent/reference_table.xlsx"
export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric18_avg_asking_rent"
rgc_source_info <- c("CoStar, Q2 2024 QTD. Calculated by Eric Clute, 8/05/2024. Includes all 1-bedroom unit data inside RGC (excludes Federal Way, Issaquah due to missing data). Rent rounded to nearest 10.")
hct_source_info <- c("CoStar, Q2 2024 QTD. Calculated by Eric Clute, 8/05/2024. Includes all 1-bedroom unit data inside HCT (excludes Port Orchard Ferry Terminal, Poulsbo, Mukilteo due to missing data). Rent rounded to nearest 10.")
county_source_info <- c("CoStar, Q2 2024 QTD. Calculated by Eric Clute, 8/05/2024. Includes all 1-bedroom unit data inside county. Rent rounded to nearest 10.")

latestdate <- "2024 Q2 QTD"
earliestdate <- "2019 Q2"

inflation_year <- (2024)
reported_year_to_adjust <- (2019)

reference_table <- read_excel(reference_path)

# Create Functions ---------------------
# Function to read Excel file, extract column names and first row, and add filename as a column
read_excel_with_filename <- function(file_path) {
  # Read Excel file
  data <- read_excel(file_path)
  
  # Extract column names and first row of data
  col_names <- colnames(data)
  first_row <- slice(data, which(data$Period %in% c(latestdate, earliestdate)))
  
  # Add filename as a column without the .xlsx extension
  filename <- gsub("\\.xlsx$", "", basename(file_path))
  first_row$name <- filename
  
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
    data_list[[i]]$first_row$name <- filename
    # Convert all columns to character type
    data_list[[i]]$first_row <- mutate_all(data_list[[i]]$first_row, as.character)
    result <- bind_rows(result, data_list[[i]]$first_row)
  }
  result
}

# Combine RGC files Together  ---------------------
xlsx_files <- list.files(rgc_data_path, pattern = "\\.xlsx$", full.names = TRUE) # Get a list of all Excel files in the folder
excel_data <- map(xlsx_files, read_excel_with_filename) # Read all Excel files
all_col_names <- excel_data %>% map("col_names") %>% reduce(union) # Combine all column names into one vector
combined_data <- tibble::tibble(!!!set_names(rep(list(NULL), length(all_col_names)), all_col_names)) # Create an empty data frame with all column names

# Call the function to combine data from all Excel files
combined_data <- combine_data(excel_data)
rgc_data <- combined_data

# Combine HCT files Together  ---------------------
xlsx_files <- list.files(hct_data_path, pattern = "\\.xlsx$", full.names = TRUE) # Get a list of all Excel files in the folder
excel_data <- map(xlsx_files, read_excel_with_filename) # Read all Excel files
all_col_names <- excel_data %>% map("col_names") %>% reduce(union) # Combine all column names into one vector
combined_data <- tibble::tibble(!!!set_names(rep(list(NULL), length(all_col_names)), all_col_names)) # Create an empty data frame with all column names

# Call the function to combine data from all Excel files
combined_data <- combine_data(excel_data)
hct_data <- combined_data

# Combine county files Together  ---------------------
xlsx_files <- list.files(county_data_path, pattern = "\\.xlsx$", full.names = TRUE) # Get a list of all Excel files in the folder
excel_data <- map(xlsx_files, read_excel_with_filename) # Read all Excel files
all_col_names <- excel_data %>% map("col_names") %>% reduce(union) # Combine all column names into one vector
combined_data <- tibble::tibble(!!!set_names(rep(list(NULL), length(all_col_names)), all_col_names)) # Create an empty data frame with all column names

# Call the function to combine data from all Excel files
combined_data <- combine_data(excel_data)
county_data <- combined_data

combined_data <- rbind(rgc_data, hct_data, county_data)

# Formatting & Calculating New Fields ---------------------

# Remove unneeded fields
combined_data <- combined_data %>%
  select(name, Period, `Inventory Units`, `Asking Rent Per Unit`)

# Cleanup
combined_data$`Asking Rent Per Unit` <- as.numeric(combined_data$`Asking Rent Per Unit`)
combined_data$`Inventory Units` <- as.numeric((combined_data$`Inventory Units`))
combined_data$`Asking Rent Per Unit` <- round(combined_data$`Asking Rent Per Unit` / 10) * 10

# Remove with less than 50 units
removed <- subset(combined_data, `Inventory Units` < 50)
combined_data <- subset(combined_data, `Inventory Units` >= 50)

# Pivot & calculate change over time
combined_data_piv <- combined_data %>% pivot_wider(id_cols = 'name', names_from = 'Period', values_from = c('Inventory Units', 'Asking Rent Per Unit'))

ed <- paste0("Asking Rent Per Unit_", earliestdate) # Construct the field name dynamically
ld <- paste0("Asking Rent Per Unit_", latestdate) # Construct the field name dynamically

combined_data_piv$prct_change_rent <- (combined_data_piv[[ld]] - combined_data_piv[[ed]]) / combined_data_piv[[ed]]

# Combine w/reference table
combined_data_piv <- left_join(combined_data_piv, reference_table, by = "name")

# Summarize by various geos ---------------------

# Summarize by Center
by_rgc <- combined_data_piv %>%
  filter(type == "rgc") %>%
  select(name, starts_with("Inventory Units_"), starts_with("Asking Rent Per Unit_"), prct_change_rent, type, county)

# Summarize by Center
by_hct <- combined_data_piv %>%
  filter(type == "hct") %>%
  select(name, starts_with("Inventory Units_"), starts_with("Asking Rent Per Unit_"), prct_change_rent, type, county)

# Summarize by Center
by_county <- combined_data_piv %>%
  filter(is.na(type)) %>%
  select(name, starts_with("Inventory Units_"), starts_with("Asking Rent Per Unit_"), prct_change_rent, type, county)

# Cleanup and export ---------------------

# Export Data
export_file <- paste0(export_path, "/metric18_raw_1bdrm.xlsx")
work_book <- createWorkbook()

# Add the "by_rgc" sheet
addWorksheet(work_book, sheetName = "by_rgc")
writeData(work_book, sheet = "by_rgc", by_rgc)
writeData(work_book, sheet = "by_rgc", x = data.frame(rgc_source_info), startRow = nrow(by_rgc) + 3, startCol = 1)

# Add the "by_hct" sheet
addWorksheet(work_book, sheetName = "by_hct")
writeData(work_book, sheet = "by_hct", by_hct)
writeData(work_book, sheet = "by_hct", x = data.frame(hct_source_info), startRow = nrow(by_hct) + 3, startCol = 1)

# Add the "by_county" sheet
addWorksheet(work_book, sheetName = "by_county")
writeData(work_book, sheet = "by_county", by_county)
writeData(work_book, sheet = "by_county", x = data.frame(county_source_info), startRow = nrow(by_county) + 3, startCol = 1)

saveWorkbook(work_book, file = export_file, overwrite = TRUE)