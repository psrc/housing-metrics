# TITLE: Housing Affordability Indexes - Median Buyer & First Time Buyer
# GEOGRAPHIES: King, Kitsap, Snohomish, Pierce Counties
# DATA SOURCE: WCRER (Washington Center for Real Estate Research)
# DATE MODIFIED: 5.15.2025
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(tidyverse)
library(readr)

save_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric16_17_affordability_indexes/metric16_17_raw.csv"

mb_hai_link <- "https://public.tableau.com/app/profile/mason.virant/viz/County_DB_HAIMedianBuyer_Q4_2024/DB-HAIMedianBuyer?County=King,Kitsap,Pierce,Snohomish"
ftb_hai_link <- "https://public.tableau.com/app/profile/mason.virant/viz/County_DB_HAIFirstTimeBuyer_Q4_2024/DB-HAIFirstTimeBuyer?County=King,Kitsap,Pierce,Snohomish"

# --------------- Download Data ---------------
mb_hai_raw <- read_csv(mb_hai_link)

 











# --------------- Median Home Buyer (HAI) ---------------
# Select County and Q1 variables
select_county <- grep("County", names(mb_hai_raw))
select_q1 <- grep("Q1", names(mb_hai_raw))

mb_hai <- mb_hai_raw %>% select(all_of(c(select_county,select_q1)))

# Select PSRC counties
mb_hai <- mb_hai[mb_hai$County %in% c("King", "Kitsap", "Pierce", "Snohomish"),]

# --------------- First-Time Home Buyer (HAI) ---------------
# Select County and Q1 variables
select_county <- grep("County", names(ftb_hai_raw))
select_q1 <- grep("Q1", names(ftb_hai_raw))

ftb_hai <- ftb_hai_raw %>% select(all_of(c(select_county,select_q1)))

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