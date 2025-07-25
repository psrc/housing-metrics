# TITLE: Median Home Price by Type
# GEOGRAPHIES: King and Snohomish Only (limited by Redfin)
# DATA SOURCE: Redfin
# DATE MODIFIED: 4.10.2025
# AUTHOR: Eric Clute

library(tidyverse)
library(openxlsx)
library(magrittr)

value_url <- "https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/redfin_metro_market_tracker.tsv000.gz"
save_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric14_median_home_price_by_type/metric14_raw.csv"

metro_area <- "Seattle, WA"
earliestdate <- "2012-07-01"
latestdate <- "2025-06-01"

# Import Redfin data, limit to metro area and by date
redfin_raw <- read_tsv(value_url)

# Limited to Metro area selected above
value <- redfin_raw %>%
  filter(str_detect(REGION, metro_area)) %>%
  transmute(
    date = PERIOD_BEGIN,
    region = REGION,
    property_type = PROPERTY_TYPE,
    median_sale_price = MEDIAN_SALE_PRICE,
    seasonally_adjusted = IS_SEASONALLY_ADJUSTED)

# limit to all residential properties, restrict to date range selected above
value <- value %>%
  filter(!(property_type == "Multi-Family (2-4 Unit)"),
         seasonally_adjusted == "TRUE")
value <- with(value, value[(date >= earliestdate & date <= latestdate), ])
value$month <- str_sub(value$date, 1, 7)

# Sort by date and property type/pivot
value <- value[order(as.Date(value$date),(factor(value$property_type, levels = c("Condo/Co-op","Single Family Residential","Townhouse","All Residential")))),]

value <- value %>%
  pivot_wider(names_from = property_type, values_from = median_sale_price)

# Filter to month of latest date
value <- subset(value, str_sub(value$month, -2,-1) == str_sub(latestdate, -5,-4))

# Export
write.csv(value,file = save_path)
