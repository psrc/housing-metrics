# TITLE: Median Home Price by Type
# GEOGRAPHIES: King and Snohomish Only (limited by Redfin)
# DATA SOURCE: Redfin
# DATE MODIFIED: 8.4.2023
# AUTHOR: Eric Clute

library(tidyverse)

redfin_raw <- read_tsv("https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/redfin_metro_market_tracker.tsv000.gz")

# Seattle Metro area only includes King and Snohomish Counties. Tacoma = Pierce, and Bremerton = Kitsap
redfin <- redfin_raw %>%
  filter(region == "Seattle, WA metro area") %>%
  transmute(
    date = period_begin,
    region = region,
    property_type = property_type,
    median_sale_price = median_sale_price
    )

# Remove Multi-Family rows
redfin <- redfin %>%
  filter(!(property_type == "Multi-Family (2-4 Unit)"))

# Sort by date and property type/pivot wide before exporting
redfin <- redfin[order(as.Date(redfin$date),(factor(redfin$property_type, levels = c("All Residential", "Single Family Residential", "Townhouse", "Condo/Co-op")))),]

redfin <- redfin %>%
  pivot_wider(names_from = date , values_from = median_sale_price)

# Exporting just Seattle Metro Data (King and Snohomish)
library(openxlsx)

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "King & Snohomish")
writeData(work_book, "King & Snohomish", redfin)
saveWorkbook(work_book, file = "Home Price by Type - Redfin/PriceByType-KingSnohomish.xlsx", overwrite = TRUE)
