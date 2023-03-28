# TITLE: Affordable Rental Housing by Tract - for each R/E Category
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS Data 2017-21
# LAST EDITED: 3.28.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)

setwd("C:/Users/eclute/Downloads")

#------------ Collect Median Gross Rent by Tract ------------
DP04Table_raw <- get_acs_recs(geography = 'tract',
                           table.names = c('DP04'),
                           years = 2021,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

grossrent <- DP04Table_raw %>%
  filter(variable == "DP04_0134") %>%
  mutate(.keep = "none",
         geoid = GEOID,
         grossrent = estimate,
         grossrent_moe = moe)

#------------ Median Renter HH Income by Race/Ethnicity Per County & Regional Total ------------

pums_raw <- get_psrc_pums(5,2020,"h",c("PRACE","TEN","HINCP"))

incbyre<- psrc_pums_median(pums_raw, "HINCP", group_vars = c("COUNTY","TEN", "PRACE"))
incbyre_region<- psrc_pums_median(pums_raw, "HINCP", group_vars = c("TEN", "PRACE"))

incbyre_county_region <- rbind(incbyre, incbyre_region)


incbyre_final <- filter(incbyre_county_region, TEN == "Rented")

incbyre_final$maxmonthlyrent <- incbyre_final$HINCP_median/12*0.3
incbyre_final$moeupperbound <- incbyre_final$HINCP_median + incbyre_final$HINCP_median_moe
incbyre_final$moelowerbound <- incbyre_final$HINCP_median - incbyre_final$HINCP_median_moe

#-------------- Write to Excel --------------
library(openxlsx)

setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

write.xlsx(incbyre_final, "MedianRenterHHIncomeByRE.xlsx")
write.xlsx(grossrent, "GrossMedianRentBy2020tract.xlsx")

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "medianrenter_inc_re")
writeData(work_book, "medianrenter_inc_re", incbyre_final)
addWorksheet(work_book, sheetName = "grosrent_tract")
writeData(work_book, "grosrent_tract", grossrent)
saveWorkbook(work_book, file = "Access to Affordable Rental Housing/r_output 2021 5YR.xlsx", overwrite = TRUE)

