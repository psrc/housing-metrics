# Cleaning and calculating housing/population growth ratios
# Geographies: Region
# Data Vintage: 2024 OFM April 1 estimates
# Created By: Eric Clute

# Assumptions ---------------------
library(dplyr)
library(openxlsx)
library(tidyverse)
library(psrchousing)

export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric01_pop_growth_hu_supply"
source_info <- c("OFM April 1 Population and Housing Estimates. Data representing 1980, 1990, 2000, 2010, 2020, 2024. Calculated by Eric Clute.")

years <- c(1980, 1990, 2000, 2010, 2020, 2024)

# Import ---------------------
hu_raw <- ofm_county_housing_unit_data()
pop_raw <- ofm_county_population_data()

# Clean up data, join ---------------------
hu <- hu_raw %>% filter(geography == "Region") %>% select(year, total) %>% rename(units = total)
pop <- pop_raw %>% filter(geography == "Region") %>% ungroup() %>% select(year, population)

analysis <- left_join(hu,pop, by = "year")

# Calculate change in pop, hu, and ratio ---------------------
analysis <- analysis %>%
  filter(year %in% years) %>%
  mutate(hu_change = units - lag(units),
         pop_change = population - lag(population),
         hu_pop_ratio = pop_change / hu_change,
         hu_per_cap_1k = hu_change / population * 1000)

# Cleanup and export ---------------------
export_file <- paste0(export_path, "/metric01_raw.xlsx")
work_book <- createWorkbook()

addWorksheet(work_book, sheetName = "analysis")
writeData(work_book, sheet = "analysis", analysis)
writeData(work_book, sheet = "analysis", x = data.frame(source_info), startRow = nrow(analysis) + 3, startCol = 1)

saveWorkbook(work_book, file = export_file, overwrite = TRUE)