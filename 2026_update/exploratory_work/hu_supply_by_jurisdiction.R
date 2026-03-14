# Exploration of tracking housing supply by jurisdiction
# Geographies: Jurisdiction
# Created By: Eric Clute

# Assumptions ---------------------
library(dplyr)
library(tidyverse)
library(psrchousing)
library(openxlsx)

export_path <- "J:/Projects/V2050/Housing/Monitoring/2026Update/exploratory_work/hu_supply_by_juris"
source_info <- c("OFM April 1 Population and Housing Estimates.")
multi_county_juris <- c("Auburn", "Bothell", "Milton", "Pacific", "Enumclaw")

# Functions ---------------------

ofm_juris_housing_unit_data <- function(dec = 0) {
  
  # Silence the dplyr summarize message
  options(dplyr.summarise.inform = FALSE)
  
  # File locations for OFM downloads 
  ofm_postcensal_hu_file = "x:/DSA/population-trends/data/housing/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx"
  ofm_intercensal_hu_file = "x:/DSA/population-trends/data/housing/ofm_april1_intercensal_estimates_2010_2020.xlsx"

  # Post-censal estimates - 2020 to present
  print(stringr::str_glue("Processing OFM post-censal housing estimates from 2020 to present"))
  ofm_postcensal_data <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_postcensal_hu_file, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Housing Units")) |>
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
   dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "Total Housing Units", "total")) |>
   dplyr::rename(geography = "Jurisdiction") |>
   dplyr::filter(variable == "total" & year >= "2020")
  
  # Inter-censal estimates - 2019 for King County (KC growth targets started in 2019)
  print(stringr::str_glue("Processing OFM inter-censal housing estimates for 2019 in King County"))
  ofm_intercensal_data <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_intercensal_hu_file, detectDates = FALSE, skipEmptyRows = TRUE, colNames = TRUE, sheet = "Total Housing")) |>
    dplyr::filter(.data$County.Name %in% c("King")) |>
    dplyr::mutate(Jurisdiction = if_else(str_detect(Jurisdiction, "\\(part\\)"), str_replace(Jurisdiction, "part", County.Name), Jurisdiction)) |>
    dplyr::select(Jurisdiction, `2019.Intercensal.Total.Housing.Unit.Estimate`) |>
    dplyr::mutate(Jurisdiction = stringr::str_replace_all(Jurisdiction, " city", "")) |>
    dplyr::mutate(Jurisdiction = stringr::str_replace_all(Jurisdiction, " town", "")) |>
    dplyr::rename(geography = "Jurisdiction") |>
    dplyr::rename(estimate = "2019.Intercensal.Total.Housing.Unit.Estimate") |>
    dplyr::mutate(variable = "total", year = 2019)
  
  # Cleanup
  print(stringr::str_glue("Combining postcensal/intercensal data and cleaning up"))
  tbl <- dplyr::bind_rows(ofm_postcensal_data, ofm_intercensal_data)|>
    tidyr::pivot_wider(names_from = "variable", values_from = "estimate") |>
    dplyr::group_by(geography) |>         # group by geography
    dplyr::arrange(year, .by_group = TRUE) |>  # order rows by year within each geography
    
    #clean up South Prairie data - OFM confirms that 112 RV park units are now categorized as mobile homes            #In 2020 population was adjusted, housing units not corrected until 2022. 
    dplyr::mutate(total = if_else(geography == "South Prairie" & year %in% c(2020, 2021), total + 112, total))|>
#    dplyr::mutate(total_annual_chg = round(total - dplyr::lag(total))) |>
    dplyr::filter(year <= current_year) |>
    dplyr::filter(!grepl("Incorporated", geography)) |>
    dplyr::filter(!grepl("^(King|Kitsap|Pierce|Snohomish) County$", geography)) |>
    dplyr::filter(!grepl("Enumclaw \\(Pierce\\)", geography)) |> #removed - no housing targets for Pierce 
    dplyr::mutate(geography = if_else(str_starts(geography, "Enumclaw"),str_replace(geography, " \\(.*\\)", ""), geography))
  
  return(tbl)
}

ofm_juris_housing_unit_change <- function() {
  # Crunch change in housing units over time
  hu <- hu_raw  |> 
    group_by(geography) |>
    summarise(first_total = total[which.min(year)],          # total for earliest year in this geo
              last_total = total[which.max(year)],           # total for latest year in this geo
              hu_change = last_total - first_total,
              years = paste(min(year), max(year), sep = "-")) |>
    ungroup()
}

ofm_annexation_data <- function() {
  # OFM annexation file
  ofm_annexation_link = "https://ofm.wa.gov/wp-content/uploads/sites/default/files/public/dataresearch/pop/annex/annex_detail.xlsx"
  
  # Formatting
  print(stringr::str_glue("Processing OFM annexation data"))
  
  ofm_annex_data_raw <- dplyr::as_tibble(openxlsx::read.xlsx(ofm_annexation_link, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Annexations"))
  
  ofm_annex_data <- ofm_annex_data_raw |>
    dplyr::filter(
      (County.Name %in% c("Kitsap","Pierce","Snohomish") & `OFM.April.1.Estimate.Year` %in% annex_years_3cnty) |
      (County.Name == "King" & `OFM.April.1.Estimate.Year` %in% annex_years_king))|>
    dplyr::mutate(dplyr::across(.cols = ends_with(".Date"), ~ as.Date(.x, origin = "1899-12-30"))) |>
    dplyr::group_by(City.Name) |>
    
    # Locate any multicounty jurisdictions, add county name to city name ex: Auburn (King)
    dplyr::mutate(is_multi_county = City.Name %in% multi_county_juris, City.Name = dplyr::if_else(is_multi_county, stringr::str_glue("{City.Name} ({County.Name})"), City.Name)) |>
    
    # Cleanup
    dplyr::select(City.Name, Total.Housing.Units) |>
    rename(annexed_hu = "Total.Housing.Units", geography = "City.Name") |>
    dplyr::group_by(geography) |>
    dplyr::summarise(annexed_hu = sum(annexed_hu, na.rm = TRUE), .groups = "drop")
  
}

hu_targets_juris <- function() {
  # File location
  targets_data <- "J:/Projects/V2050/Housing/Monitoring/2026Update/exploratory_work/hu_supply_by_juris/Regional Housing Need by Income Band.xlsx"
  
  # Read in housing targets data
  targets_raw <- read.xlsx(targets_data, sheet = "Combined")
  
  # Clean up
  targets <- targets_raw |> 
    dplyr::select(Jurisdiction, County, Regional.Geography, Growth_Total) |>
    rename(geography = Jurisdiction, growth_target = Growth_Total, type = Regional.Geography) |>
    
    mutate(base_annualized = dplyr::if_else(County == "King",
                                                   growth_target / 25,
                                                   growth_target / 24))|>
    
    mutate(annualized_growth_target = dplyr::case_when(
      
             # Special case: regional row
             geography == "Region" & (is.na(County) | County == "") ~
               sum(base_annualized[County == "King"], na.rm = TRUE) +
               sum(base_annualized[County != "King" & !is.na(County)], na.rm = TRUE),
             
             # All normal rows
             TRUE ~ base_annualized)) |>
    mutate(annualized_growth_target = round(annualized_growth_target, 1),
           type = factor(dplyr::coalesce(type, "Region"), levels = c("Region", "Metro", "Core", "HCT", "C+T", "UU"))) |>
  
    dplyr::select(-base_annualized)

}

join_data <- function() {
  year_label <- paste0("as_of_", current_year)
  
  net_col <- paste0(year_label, "_net_hu")
  perc_col <- paste0(year_label, "_perc_of_target")
  
  hu <- hu |> 
    dplyr::bind_rows(
      tibble(
        geography = "Region",
        first_total = sum(hu$first_total, na.rm = TRUE),
        last_total  = sum(hu$last_total, na.rm = TRUE),
        hu_change   = sum(hu$hu_change, na.rm = TRUE),
        years       = year_label)) |>
    left_join(targets, by = "geography") |>
    left_join(annex, by = "geography") |>
    dplyr::mutate(
      "{net_col}" := hu_change - dplyr::coalesce(annexed_hu, 0),
      # Round net_hu to nearest 5 unless <20
      "{net_col}" := if_else(.data[[net_col]] < 20,
                             .data[[net_col]],
                             round(.data[[net_col]] / 5) * 5),
      "{perc_col}" := .data[[net_col]] / growth_target)|>
    select(geography, type, years, growth_target, dplyr::matches("\\d+_net_hu$"), dplyr::ends_with("_perc_of_target")) |>
    arrange(type, desc(.data[[perc_col]]))
}

# Since Targets were Adopted Analysis ------------------

current_year <- (2025)
annex_years_king <- (2020:current_year) #Extra year of annexations for King due to longer housing target period
annex_years_3cnty <- (2021:current_year) #Should be 1 less year than the housing target period. EX: timespan = 2020:2025 and annex_years = 2021:2025

# Clean data
hu_raw <- ofm_juris_housing_unit_data()
targets <- hu_targets_juris()
annex <- ofm_annexation_data()

# Calculate change in units - OFM housing unit data
hu <- ofm_juris_housing_unit_change()

# Join and analyze
multi_year <- join_data()

# Export ------------------
openxlsx::write.xlsx(multi_year, file = file.path(export_path, "export_analysis_tbl.xlsx"), rowNames = FALSE)
