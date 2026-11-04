# Retrieve published ACS data for the PSRC Regional Housing Needs Assessment
library(psrccensus)
library(magrittr)

survey_years <- c(2014, 2019, 2024)

get_acs_tbl <- function(tn){
  rs <- psrccensus::get_acs_recs(
    table.names = tn,
    geography = "county", 
    years = survey_years, 
    acs.type = 'acs5')
}

get_acs_vars <- function(v){
  rs <- psrccensus::get_acs_recs(
    variables = v,
    geography = "county", 
    years = survey_years, 
    acs.type = 'acs5',
    table.names = NULL)
}

write_rhna_acs <- function(result_list){
  openxlsx::write.xlsx(result_list, file = "rhna_acs.xlsx",
                       sheetName = names(result_list), rowNames = FALSE)
  return(invisible(NULL))
}

acs_vars <- tibble::tibble(
  table_name = c(
     "1c-Population by Age",
     "2b-HH Type",
     "3ab-Average HH Size",
     "4ab-Housing Tenure",
     "4c-Housing Tenure by Age",
     "4e-Housing Tenure by Income",
     "5b-Median HH Income",
     "5c-Median HH Income by Age",
     "7ab-Units by Tenure",
     "9a-Tenure by Bedrooms"
  ),
  variables = list(
    paste0("DP05_00", sprintf("%02d", 4:17)),
    paste0("S2501_C01_0", sprintf("%02d", c(9:10, 14:15, 19, 23:24, 28))),
    "B25010_001",
    paste0("B25007_0", sprintf("%02d", c(1, 2, 12))),
    paste0("B25007_0", sprintf("%02d", setdiff(3:21, 12))),
    paste0("B25118_0", sprintf("%02d", setdiff(3:25, 14))),
    "B19013_001",
    paste0("B19049_00", 2:5),
    paste0("B25032_0", sprintf("%02d", setdiff(3:23, 13))),
    paste0("B25042_0", sprintf("%02d", setdiff(3:15, 9))) 
  )
)  

# Pull all variable-specific data
results <- lapply(acs_vars$variables, get_acs_vars)
names(results) <- acs_vars$table_name

# Table DP05 has inconsistent variable names
# -- We request the inclusive set for each year (IDs specified above)
# -- Then rely on label to drop the variables we don't want
results$`1c-Population by Age` %<>% dplyr::filter_out(grepl("(Median|ratio)", label))

# This table must be requested whole because year_built variables differ by survey_year
results$`8ab-Tenure by Year Built` <- get_acs_tbl(tn = "B25036") %>%
  dplyr::filter_out(variable %in% paste0("B25036", sprintf("%02d", c(1, 2, 13))))

write_rhna_acs(results)
