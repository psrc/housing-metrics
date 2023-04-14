# TITLE: Median Home Price by Type
# GEOGRAPHIES: King and Snohomish Only (limited by Redfin)
# DATA SOURCE: Redfin
# DATE MODIFIED: 4.14.2023
# AUTHOR: Eric Clute

library(tidyverse)

redfin_all <- read_tsv("https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/redfin_metro_market_tracker.tsv000.gz")

# Seattle Metro area only includes King and Snohomish Counties. Tacoma = Pierce, and Bremerton = Kitsap
seattle <- redfin_all %>%
  filter(region == "Seattle, WA metro area") %>%
  transmute(
    date = period_begin,
    region = region,
    property_type = property_type,
    median_sale_price = median_sale_price
    )

# Remove Multi-Family rows, sort by date & property_type
seattle <- seattle %>%
  filter(!(property_type == "Multi-Family (2-4 Unit)"))

seattle <- seattle[order( seattle[,1], seattle[,3] ),]

# Delete every 2nd row starting from 1 - keeps only the first row for each property_type (it's nicely rounded by Redfin)
seattle <- seattle %>%filter(!(row_number() %% 2 != 1))

# Sort by date and property type/pivot wide before exporting
seattle <- seattle[order(as.Date(seattle$date),(factor(seattle$property_type, levels = c("All Residential", "Single Family Residential", "Townhouse", "Condo/Co-op")))),]

seattle <- seattle %>%
  pivot_wider(names_from = date , values_from = median_sale_price)

# Exporting just Seattle Metro Data (King and Snohomish)
library(openxlsx)

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "King & Snohomish")
writeData(work_book, "King & Snohomish", seattle)
saveWorkbook(work_book, file = "Median Home Price - Redfin/PriceByType-KingSnohomish.xlsx", overwrite = TRUE)
