# Data Wrangling Libraries
library(dplyr)
library(stringr)
library(tidyr)

# Census Data Libraries
library(censusapi)

# Database Connection Libraries
library(odbc)
library(DBI)

# Setup for Census API Key
Sys.setenv(CENSUS_KEY='enter your api key here')

# Basic Inputs
acs_years <- seq(2010, 2019, by = 1)
acs_data_type <- "acs/acs1"

gross_rent_table <- "B25111"
rent_distribution_table <- "B25063"

# SQL Database Connection settings
elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes"
)

var_dim_table <- "census.variable_dim"


# Useful Functions --------------------------------------------------------

download_county_msa_census_tables <- function(census_table, working_years, working_type) {
  
  # Download Rental Distribution Tables from Census API and combine into 1 table
  final_result <- NULL
  
  for (acs_year in working_years) {
    
    # Download the Census Data for Counties
    working_county <- as_tibble(getCensus(name = working_type,
                                          vintage = acs_year,
                                          vars = c("NAME",paste0("group(",census_table,")")),
                                          region = "county:033,035,053,061",
                                          regionin = "state:53"))
    
    drop.cols <- c("state","county","GEO_ID")
    working_county <- working_county %>% 
      select(-one_of(drop.cols)) %>%
      select_if(~ !any(is.na(.)))
    
    # Download the Census Data for MSA's
    working_msa <- as_tibble(getCensus(name = working_type,
                                       vintage = acs_year,
                                       vars = c("NAME",paste0("group(",census_table,")")),
                                       region = " metropolitan statistical area/micropolitan statistical area: 14740, 42660"))
    
    drop.cols <- c("metropolitan_statistical_area_micropolitan_statistical_area", "GEO_ID")
    working_msa <- working_msa %>% 
      select(-one_of(drop.cols)) %>%
      select_if(~ !any(is.na(.)))
    
    # Download the Metadata for the Data to provide titles
    working_labels <- listCensusMetadata(name = working_type,
                                         vintage = acs_year,
                                         type = "variables",
                                         group = census_table)
    
    working_labels <- working_labels %>%
      select(name,label)
    
    # Convert Wide to Long
    county_data_long <- working_county %>% pivot_longer(-NAME)
    msa_data_long <- working_msa %>% pivot_longer(-NAME)
    
    # Add Variable Name to long form tables
    county_data_long <- inner_join(county_data_long, working_labels, by = "name")
    msa_data_long <- inner_join(msa_data_long, working_labels, by = "name")
    
    # Remove Rows with no Values (variables that are not available) and tidy up
    county_data_long <- county_data_long %>% 
      filter(!is.na(value)) %>%
      mutate(data_year = acs_year, geography_type = "co", table = census_table)
    
    msa_data_long <- msa_data_long %>% 
      filter(!is.na(value)) %>%
      mutate(data_year = acs_year, geography_type = "msa", table = census_table)
    
    # Combine County and MSA into One Long Form Tibble
    working_total <- bind_rows(county_data_long,msa_data_long)
    
    if (is.null(final_result)) {final_result <- working_total} else {final_result <- bind_rows(final_result,working_total)}
    
  }
  
  # Remove Unused Files from memory
  rm("working_county","working_msa", "working_total","working_labels","county_data_long","msa_data_long") 
  
  return(final_result)
  
}

# Census Data Downloads ---------------------------------------------------

median_gross_rent <- download_county_msa_census_tables(gross_rent_table, acs_years, acs_data_type)
rental_cost_distribution <- download_county_msa_census_tables(rent_distribution_table, acs_years, acs_data_type)
all_census_tables <- bind_rows(median_gross_rent, rental_cost_distribution)

# Modify Tables for Elmer -------------------------------------------------

current_variable_dim <- as_tibble(dbReadTable(elmer_connection,SQL(var_dim_table)))
dbDisconnect(elmer_connection)

current_variable_dim <- current_variable_dim %>%
  filter(census_table_code == rent_distribution_table)

# Create the Variable Dimensions Table for inclusion in Elmer
updated_variable_dim <- all_census_tables %>%
  filter(geography_type == "co") %>%
  filter(NAME == "King County, Washington") %>%
  filter(grepl("[E]$", variable)) %>%
  select(variable,label,data_year,table) %>%
  rename(name = variable, label = label, census_year = data_year, census_table_code = table) %>%
  mutate(name = str_replace(name, "[E]", "")) %>%
  mutate(census_product = "1yr", category = label, sub_category = label, variable_description = label, used_in_member_profiles = FALSE) %>%
  mutate(category = str_replace(category, "Estimate!!", "")) %>%
  mutate(category = str_replace(category, "!..*", "")) %>%
  mutate(category = str_replace(category, ":", "")) %>%
  mutate(category = str_replace(category, " --", "")) %>%
  mutate(variable_description = str_replace(variable_description, ".*!", "")) %>%
  mutate(variable_description = str_replace(variable_description, ":", ""))

