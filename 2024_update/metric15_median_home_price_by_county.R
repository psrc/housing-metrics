# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin (https://public.tableau.com/shared/S6Y4NN6MH?:showVizHome=no)
# DATE MODIFIED: 7.12.2024
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(openxlsx)

save_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric15_median_home_price_by_county/metric15_raw.csv"
month <- "Feb"

# Pull in data downloaded from tableau dashboard (make sure all 4 counties are visible before exporting to CSV)
county_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric15_median_home_price_by_county/med_sale_price.csv", skip = 1)

# Filter to month
regex_pattern <- paste0("^Region|^", month, "\\.\\d+$")
selected_columns <- grep(regex_pattern, colnames(county_raw), value = TRUE)
county <- county_raw[, selected_columns]

# Clean up price data (remove "k"s, "$"s, and add "0"s)
regex_pattern <- paste0("^", month, "\\.")
selected_columns <- grep(regex_pattern, colnames(county), value = TRUE)

for (col in selected_columns) {
  # Remove the first and last character
  county[[col]] <- substr(county[[col]], 2, nchar(county[[col]]) - 1)
  
  # Convert to numeric and multiply by 1000
  county[[col]] <- as.numeric(county[[col]]) * 1000
}

# Export
write.csv(county, file = save_path)