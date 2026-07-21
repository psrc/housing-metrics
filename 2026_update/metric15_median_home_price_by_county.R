# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin (https://public.tableau.com/shared/GRN635HN7?:display_count=n&:origin=viz_share_link)
# DATE MODIFIED: 7.21.2026
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

data_path <- "J:/Projects/V2050/Housing/Monitoring/20256Update/data/metric15_median_home_price_by_county/redfin_housing_market_monthly_top_50_metros_key_metrics_2012_Jun_to_2026_Jun.csv"
data_path <- "J:/Projects/V2050/Housing/Monitoring/20256Update/data/metric15_median_home_price_by_county/redfin_property_types_monthly_top_50_metros_key_metrics_2012_Jun_to_2026_Jun.csv"

save_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric15_median_home_price_by_county/metric15_raw.csv"



# Export
write.csv(county, file = save_path)
