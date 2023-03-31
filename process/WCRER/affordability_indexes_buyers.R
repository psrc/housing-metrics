# TITLE: Housing Affordability Indexes - Median Buyer & First Time Buyer
# GEOGRAPHIES: King, Kitsap, Snohomish, Pierce Counties
# DATA SOURCE: WCRER (Washington Center for Real Estate Research)
# DATE MODIFIED: 3.31.2023
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(tidyverse)

setwd("C:/Users/eclute/Downloads")

# Download Data from WCRER site - will need to update link below for newer data
url <- "https://wcrer.be.uw.edu/wp-content/uploads/sites/60/2023/02/County-Housing-Affordability-Index-Report-4Q2022.xlsx"
mb_hai_raw = read.xlsx(url, sheet=2)
ftb_hai_raw = read.xlsx(url, sheet=3)

# --------------- Median Home Buyer (HAI) ---------------
# Select County and Q1 variables
select_county <- grep("County", names(mb_hai_raw))
select_q1 <- grep("Q1", names(mb_hai_raw))

mb_hai <- mb_hai_raw %>% select(c(select_county,select_q1))

# Select PSRC counties
mb_hai <- mb_hai[mb_hai$County %in% c("King", "Kitsap", "Pierce", "Snohomish"),]

# --------------- First-Time Home Buyer (HAI) ---------------
# Select County and Q1 variables
select_county <- grep("County", names(ftb_hai_raw))
select_q1 <- grep("Q1", names(ftb_hai_raw))

ftb_hai <- ftb_hai_raw %>% select(c(select_county,select_q1))

# Select PSRC counties
ftb_hai <- ftb_hai[ftb_hai$County %in% c("King", "Kitsap", "Pierce", "Snohomish"),]

# --------------- Export to Excel ---------------
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "first_time_buyer_hai")
writeData(work_book, "first_time_buyer_hai", ftb_hai)
addWorksheet(work_book, sheetName = "median_buyer_hai")
writeData(work_book, "median_buyer_hai", mb_hai)
saveWorkbook(work_book, file = "Affordability Measures/r_output_HAI_Q1.xlsx", overwrite = TRUE)