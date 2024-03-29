# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin February 2023 (https://public.tableau.com/shared/JJ67ZXN9N)
# DATE MODIFIED: 3.30.2023
# AUTHOR: Eric Clute



library(dplyr)
library(stringr)
library(openxlsx)

# Pull in data downloaded from tableau dashboard (make sure all 4 counties are visible before exporting to CSV)
county_raw <- read.csv("J:/Projects/V2050/Housing/Monitoring/2023Update/Metrics/Affordability Measures/med_sale_price_data_feb2023.csv")

# Filter - select only Feb records
county <- county_raw  %>%
  filter(str_detect(Month.of.Period.End, (str_c(c("^Feb"), collapse="|"))))

# Clean up price data (remove "k"s, "$"s, and add "0"s)
county$Avg..Median.Sale.Price <- stringr::str_replace(county$Avg..Median.Sale.Price,"K", "")
county$Avg..Median.Sale.Price <- str_sub(county$Avg..Median.Sale.Price, -3, -1)
county$Avg..Median.Sale.Price <- str_pad(county$Avg..Median.Sale.Price,width = 6, side = "right", pad = "0")
county$Avg..Median.Sale.Price <- as.numeric(county$Avg..Median.Sale.Price)

# Pivot for chart
county_piv <- county %>% 
  pivot_wider(names_from = Month.of.Period.End, values_from = Avg..Median.Sale.Price)

# Counties in alphabetical order
county_piv <- arrange(county_piv, Region, desc('numeric'))

# Export data

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "median sale price")
writeData(work_book, "median sale price", county_piv)
saveWorkbook(work_book, file = "Affordability Measures/r_output median_price_county.xlsx", overwrite = TRUE)
