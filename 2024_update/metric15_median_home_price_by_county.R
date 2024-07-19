# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin (https://public.tableau.com/shared/S6Y4NN6MH?:showVizHome=no)
# DATE MODIFIED: 7.18.2024
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(openxlsx)

data_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric15_median_home_price_by_county/med_sale_price_Full Data_data.csv"
save_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric15_median_home_price_by_county/metric15_raw.csv"
month <- "June"

# Pull in data downloaded from tableau dashboard (make sure all 4 counties are visible before exporting to CSV)
county_raw <- read.csv(data_path)

# Filter to month & pivot
county_raw <- county_raw[startsWith(county_raw$Month.of.Period.End, month), ]
county_raw <- county_raw[order(county_raw$Month.of.Period.End), ]
county_piv <- county_raw %>% pivot_wider(names_from = Month.of.Period.End, values_from = Median.Sale.Price)

# Remove unneeded fields
county_piv <- county_piv %>% select(-c(Property.Type,Region.Type,Seasonally.Adjusted,State.Code,Worksheet.Filter))

# Identify columns that start with the specified month
month_columns <- grep(paste0("^", month), names(county_piv), value = TRUE)

# Loop through each identified column and modify the data
for (col in month_columns) {
  # Convert column to character type to safely manipulate strings
  county_piv[[col]] <- as.character(county_piv[[col]])
  
  # Remove "$" and "k", append ",000", and convert to numeric
  county_piv[[col]] <- gsub("\\$", "", gsub("K", "000", county_piv[[col]]))
  
  # Convert column to character type to safely manipulate strings
  county_piv[[col]] <- as.numeric(county_piv[[col]])
}

# Export
write.csv(county_piv, file = save_path)