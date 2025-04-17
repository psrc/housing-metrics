# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin (https://public.tableau.com/shared/GRN635HN7?:display_count=n&:origin=viz_share_link)
# DATE MODIFIED: 4.17.2025
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)

data_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric15_median_home_price_by_county/data_redfin.csv"
save_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric15_median_home_price_by_county/metric15_raw.csv"
month <- "Feb"

# Pull in data downloaded from tableau dashboard (make sure all 4 counties are visible before exporting to CSV)
county_raw <- read.csv(data_path)

# Remove unneeded fields
county <- county_raw %>% select(c(Region, Month.of.Period.End,Median.Sale.Price))

# Clean up numeric data
county$Median.Sale.Price <- gsub("\\$", "", gsub("K", "000", county$Median.Sale.Price))

# Filter to month & pivot
county <- county[startsWith(county$Month.of.Period.End, month), ]
county_piv <- county %>% pivot_wider(names_from = Region, values_from = Median.Sale.Price)

# Export
write.csv(county_piv, file = save_path)