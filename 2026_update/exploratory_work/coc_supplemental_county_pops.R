# Title: Exploration of homelessness rates/unsheltered rates
## Suplemental Script: PSRC Region Population Estimates
# Geographies: PSRC Region
# Created By: Eric Clute

# Assumptions ---------------------
library(dplyr)
library(tidyverse)
library(psrchousing)
library(openxlsx)

# Clean OFM data ----------------------

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# File locations for OFM downloads 
ofm_postcensal_pop_file = "x:/DSA/population-trends/data/population/ofm_april1_population_2020_2024.xlsx"
ofm_intercensal_pop_file = "x:/DSA/population-trends/data/population/ofm_april1_intercensal_estimates_county_1960-2020.xlsx"

# Postcensal estimates - 2020 to present
print(stringr::str_glue("Processing OFM postcensal population estimates from 2020 to present"))
ofm_postcensal_data <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_postcensal_pop_file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Population")) |>
  dplyr::filter(.data$Jurisdiction %in% c("King County","Kitsap County","Pierce County","Snohomish County")) |>
  dplyr::select(-"Line", -"Filter", -"County") |>
  dplyr::rename_with(
    ~ stringr::str_extract(.x, "\\d{4}"),
    -dplyr::all_of("Jurisdiction")) |>
  dplyr::mutate(dplyr::across(-dplyr::all_of("Jurisdiction"), as.numeric))

# Intercensal estimates - 2010 to 2019
print(stringr::str_glue("Processing OFM intercensal population estimates from 2010 to 2019"))
ofm_intercensal_data <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_intercensal_pop_file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Population")) |>
  dplyr::filter(.data$Jurisdiction %in% c("King County","Kitsap County","Pierce County","Snohomish County")) |>
  dplyr::select(-"Line", -"Filter", -"County") |>
  dplyr::rename_with(
    ~ stringr::str_remove(.x, "^Intercensal\\.Estimate\\.of\\.Total\\.Population\\.") |>
      stringr::str_remove("^Census\\.Count\\.of\\.Total\\.Population\\."),
    -dplyr::all_of("Jurisdiction")) |>
  dplyr::select(Jurisdiction, `2010`:`2019`)
  
# Cleanup
print(stringr::str_glue("Combining postcensal/intercensal data and cleaning up"))
county_pop <- left_join(ofm_intercensal_data, ofm_postcensal_data, by = "Jurisdiction") |>
  pivot_longer(cols = -Jurisdiction, names_to = "year", values_to = "population") |>
  mutate(year = as.integer(year))
rm(ofm_intercensal_data, ofm_postcensal_data, ofm_intercensal_pop_file, ofm_postcensal_pop_file)
