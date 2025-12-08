# Exploration of tracking housing supply by jurisdiction
# Geographies: Jurisdiction
# Data Vintage: 2025 OFM April 1 estimates
# Created By: Eric Clute

# Assumptions ---------------------
library(dplyr)
library(openxlsx)
library(tidyverse)
library(psrchousing)

export_path <- "J:/Projects/V2050/Housing/Monitoring/2026Update/exploratory_work/hu_supply_by_juris"
source_info <- c("OFM April 1 Population and Housing Estimates.")

# Function ---------------------

ofm_juris_housing_unit_data <- function(dec = 0) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # File locations for OFM downloads 
  ofm_hu_file="x:/DSA/population-trends/data/housing/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx"

  # 1980 to present
  print(stringr::str_glue("Processing OFM post-censal housing estimates from 1980 to present"))
  ofm_data <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_hu_file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Housing Units")) |>
    dplyr::filter(.data$County %in% c("King","Kitsap","Pierce","Snohomish")) |>
    tidyr::pivot_longer(cols=dplyr::contains("Housing"), names_to="temp", values_to="estimate") |>
    dplyr::mutate(Jurisdiction = if_else(str_detect(Jurisdiction, "\\(part\\)"), str_replace(Jurisdiction, "part", County), Jurisdiction)) |>
    dplyr::select(-"Line", -"Filter", -"County") |>
    tidyr::separate(col = .data$temp, sep = 4, into = c("year", "variable"), remove = TRUE) |>
   dplyr::mutate(variable = stringr::str_remove_all(.data$variable, ".Census.Count.of.")) |>
   dplyr::mutate(variable = stringr::str_remove_all(.data$variable, ".Postcensal.Estimate.of.")) |>
   dplyr::mutate(variable = stringr::str_remove_all(.data$variable, ".Census-Based.Estimate.of.")) |>
   dplyr::mutate(variable = stringr::str_remove_all(.data$variable, "\U00B9")) |>
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "\\.", " ")) |>
   dplyr::mutate(estimate = stringr::str_replace_all(.data$estimate, "\\.", " ")) |>
   dplyr::mutate(year = as.numeric(.data$year), estimate = as.numeric(.data$estimate)) |>
   dplyr::mutate(estimate = tidyr::replace_na(.data$estimate, 0)) |>
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "One Unit Housing Units", "sf")) |>
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Two or More Housing Units", "mf")) |>
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Mobile Home and Special Housing Units", "mh")) |>
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Total Housing Units", "total")) |>
   dplyr::rename(geography = "Jurisdiction")
  
  print(stringr::str_glue("Summarizing OFM post-censal housing estimates from 1980 to present for PSRC region"))
  region <- ofm_data |>
    dplyr::group_by(.data$year, .data$variable) |>
    dplyr::summarise(estimate = sum(.data$estimate)) |>
    dplyr::as_tibble() |>
    dplyr::mutate(geography = "Region")
  
  # Cleanup
  print(stringr::str_glue("Combining Jurisdiction and Region tibbles and cleaning up"))
  tbl <- dplyr::bind_rows(ofm_data, region) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "estimate") |>
    dplyr::group_by(.data$geography) |>
    dplyr::mutate(total_annual_chg = round(.data$total - dplyr::lag(.data$total), dec)) |>
    dplyr::mutate(sf_chg = round(.data$sf - dplyr::lag(.data$sf), dec)) |>
#    dplyr::mutate(sf_per = .data$sf_chg / .data$total_chg) |>
    dplyr::mutate(mf_chg = round(.data$mf - dplyr::lag(.data$mf), dec)) |>
#    dplyr::mutate(mf_per = .data$mf_chg / .data$total_chg) |>
    dplyr::mutate(mh_chg = round(.data$mh - dplyr::lag(.data$mh), dec)) |>
#    dplyr::mutate(mh_per = .data$mh_chg / .data$total_chg) |>
    dplyr::select("year", "geography",
                  "sf", "sf_chg",# "sf_per",
                  "mf", "mf_chg",# "mf_per",
                  "mh", "mh_chg",# "mh_per",
                  "total", "total_annual_chg") |>
    dplyr::mutate(geography = stringr::str_remove_all(.data$geography, " County")) |>
    dplyr::arrange(.data$geography, .data$year) |>
    dplyr::filter(year %in% 2024:2025 ) |>
    dplyr::filter(!grepl("Incorporated", geography)) |>
    dplyr::filter(!grepl("^(King|Kitsap|Pierce|Snohomish)$", geography)) |>
    dplyr::filter(!grepl("Enumclaw \\(Pierce\\)", geography)) |> #removed - no housing units on Pierce side of Enumclaw
    dplyr::mutate(geography = if_else(str_starts(geography, "Enumclaw"),str_replace(geography, " \\(.*\\)", ""), geography))
  
  return(tbl)
}

ofm_juris_housing_unit_change <- function() {
  # Crunch change in housing units over time
  hu <- hu_raw %>% dplyr::select(year, geography, total) %>%
    group_by(geography) %>%
    summarise(first_total = total[which.min(year)], last_total = total[which.max(year)], change_total = last_total - first_total, years = paste(min(year), max(year), sep = "-")) %>%
    ungroup()
}

hu_targets_juris <- function() {
  # File location
  targets_data <- "J:/Projects/V2050/Housing/Monitoring/2026Update/exploratory_work/hu_supply_by_juris/Regional Housing Need by Income Band.xlsx"
  
  # Read in housing targets data
  targets_raw <- read.xlsx(targets_data, sheet = "Combined")
  
  # Clean up
  targets <- targets_raw %>% dplyr::select(Jurisdiction, Growth_Total) %>%
    rename(geography = Jurisdiction)

}

join_ofm_and_targets <- function() {}


analysis <- hu %>% left_join(targets, by = "geography") %>%
  mutate(perc_total_built = change_total / Growth_Total)

# Analysis ------------------

hu_raw <- ofm_juris_housing_unit_data()
hu <- ofm_juris_housing_unit_change()
targets <- hu_targets_juris()
