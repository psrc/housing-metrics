# TITLE: Affordable Rental Housing by Tract - for each R/E Category
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS Data 2017-21
# LAST EDITED: 3.31.2023
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

#------------ Median Renter HH Income by Race/Ethnicity ------------
pums_raw <- get_psrc_pums(5,2021,"h",c("PRACE","TEN","HINCP"))

# Create/modify variables

pums <- pums_raw %>% mutate(PRACE=factor(
  case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
            grepl("^Black ", PRACE) ~"Black",
            grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
            !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
            stringr::str_replace(" alone", "") %>%
            stringr::str_replace(" Alone", ""))))

incbyre<- psrc_pums_median(pums, "HINCP", group_vars = c("TEN", "PRACE"))
incbyre <- filter(incbyre, TEN == "Rented")

# Create new fields, calculate cost max rent, moe upper/lower, and RR (relative reliability) score
incbyre$maxmonthlyrent <- incbyre$HINCP_median/12*0.3
incbyre$moeupperbound <- incbyre$HINCP_median + incbyre$HINCP_median_moe
incbyre$moelowerbound <- incbyre$HINCP_median - incbyre$HINCP_median_moe
incbyre$rr_score <- (incbyre$HINCP_median_moe/1.645)/incbyre$HINCP_median*100

# Create RE reference table
incbyre <- incbyre[incbyre$PRACE %in% c("Asian", "Black", "Hispanic/Latinx", "White"),]
incbyre_piv <- incbyre[, c(4,7)]
incbyre_piv <- incbyre_piv %>% pivot_wider(names_from = PRACE, values_from = maxmonthlyrent)

#-------------- Indicate which tracts are affordable to each R/E category --------------

grossrent$affordable_asian <- ifelse(incbyre_piv$Asian >= grossrent$grossrent, 1, 0)
grossrent$affordable_black <- ifelse(incbyre_piv$Black >= grossrent$grossrent, 1, 0)
grossrent$affordable_hispanic <- ifelse(incbyre_piv$`Hispanic/Latinx` >= grossrent$grossrent, 1, 0)
grossrent$affordable_white <- ifelse(incbyre_piv$White >= grossrent$grossrent, 1, 0)

#-------------- Write to Excel --------------
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

write.csv(grossrent, "Access to Affordable Rental Housing/r_output 2021 5YR.csv", row.names=FALSE)