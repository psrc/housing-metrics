# TITLE: Affordable Rental Housing by Tract - for all renters
# GEOGRAPHIES: PSRC Region & Census Tract
# DATA SOURCE: 5YR ACS Data
# LAST EDITED: 5.15.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)

year <- (2014)
setwd("C:/Users/eclute/Downloads")

#------------ Collect Median Gross Rent by Tract ------------
DP04Table_raw <- get_acs_recs(geography = 'tract',
                           table.names = c('DP04'),
                           years = year,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

grossrent <- DP04Table_raw %>%
  filter(ifelse(year >= '2015', variable == "DP04_0134", variable == "DP04_0132")) %>% #variable change beginning in 2015
  mutate(.keep = "none",
         DATA_YEAR = year,
         geoid = GEOID,
         variable = variable,
         grossrent = estimate,
         grossrent_moe = moe)

# Evaluate RR scores - can we trust these data?
grossrent$rr_score <- (grossrent$grossrent_moe/1.645)/grossrent$grossrent*100
grossrent <- grossrent %>%
  mutate(rr_score=factor(case_when(rr_score <= 15 ~"good",
                                   rr_score <= 30 ~"fair",
                                   rr_score <= 50 ~"weak",
                                   rr_score > 50 ~"unreliable",
                                   !is.na(rr_score) ~ NA)))

#------------ Median Renter HH Income by Race/Ethnicity ------------
setwd("C:/Users/eclute/Downloads")
pums_raw <- get_psrc_pums(5,year,"h",c("TEN","HINCP"))

# Create/modify variables
pums <- pums_raw

incbyre <- psrc_pums_median(pums, "HINCP", group_vars = c("DATA_YEAR","TEN"),rr=TRUE)
incbyre <- filter(incbyre, TEN == "Rented")

# Create new fields, calculate cost max rent, moe upper/lower
incbyre$maxmonthlyrent <- incbyre$HINCP_median/12*0.3
incbyre$moeupperbound <- incbyre$HINCP_median + incbyre$HINCP_median_moe
incbyre$moelowerbound <- incbyre$HINCP_median - incbyre$HINCP_median_moe

# Create reference table
incbyre_piv <- incbyre
incbyre_piv <- incbyre_piv %>% pivot_wider(names_from = COUNTY, values_from = maxmonthlyrent)

#-------------- Indicate which tracts are affordable --------------
grossrent$affordable <- ifelse(incbyre_piv$Region >= grossrent$grossrent, 1,0)

#-------------- Summary of census tracts affordable -------------
summarytbl <- data.frame(year)
summarytbl$affordable <- sum(na.omit(grossrent$affordable))/nrow(na.omit(grossrent))

#-------------- Write to Excel --------------
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

write.csv(grossrent, "Access to Affordable Rental Housing/r_output.csv", row.names=FALSE)