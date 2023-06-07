# TITLE: Affordable Rental Housing by Tract - for each R/E Category
# GEOGRAPHIES: PSRC Region & Census Tract
# DATA SOURCE: 5YR ACS Data
# LAST EDITED: 6.2.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)
library(fredr)
library(stringr)

year <- (2010)
inflation_year <- (2021)
file_path <- "J:/Projects/V2050/Housing/Monitoring/2023Update/Access to Affordable Rental Housing/r_output 2010 5YR Adjusted.csv"

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

# Adjust for inflation
grossrent$inflation_adjust <- pce_deflator(year, inflation_year)
grossrent$grossrent <- grossrent$grossrent * grossrent$inflation_adjust
grossrent$grossrent_moe <- grossrent$grossrent_moe * grossrent$inflation_adjust
grossrent = grossrent[,!grepl("inflation_adjust",names(grossrent))]

# Evaluate RR scores - can we trust these data?
grossrent$rr_score <- (grossrent$grossrent_moe/1.645)/grossrent$grossrent*100
grossrent <- grossrent %>%
  mutate(rr_score=factor(case_when(rr_score <= 15 ~"good",
                                   rr_score <= 30 ~"fair",
                                   rr_score <= 50 ~"weak",
                                   rr_score > 50 ~"unreliable",
                                   !is.na(rr_score) ~ NA)))

#------------ Median Renter HH Income by Race/Ethnicity ------------
pums_raw <- get_psrc_pums(5,year,"h",c("PRACE","TEN","HINCP"))
pums_raw <- real_dollars(pums_raw, inflation_year)

# Create/modify variables

pums <- pums_raw %>% mutate(PRACE=factor(
  case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
            grepl("^Black ", PRACE) ~"Black",
            grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
            !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
            stringr::str_replace(" alone", "") %>%
            stringr::str_replace(" Alone", ""))))

incbyre <- psrc_pums_median(pums, "HINCP2021", group_vars = c("DATA_YEAR","TEN", "PRACE"),rr=TRUE)
incbyre <- filter(incbyre, TEN == "Rented")

incbyre <- incbyre %>% rename("HINCP_median" = "HINCP2021_median")
incbyre <- incbyre %>% rename("HINCP_median_moe" = "HINCP2021_median_moe")

# Create new fields, calculate cost max rent, moe upper/lower
incbyre$maxmonthlyrent <- incbyre$HINCP_median/12*0.3
incbyre$moeupperbound <- incbyre$HINCP_median + incbyre$HINCP_median_moe
incbyre$moelowerbound <- incbyre$HINCP_median - incbyre$HINCP_median_moe

# Create RE reference table
incbyre <- incbyre[incbyre$PRACE %in% c("Asian", "Black", "Hispanic/Latinx", "White"),]
incbyre_piv <- incbyre[, c(4,8)]
incbyre_piv <- incbyre_piv %>% pivot_wider(names_from = PRACE, values_from = maxmonthlyrent)

#-------------- Indicate which tracts are affordable to each RE category --------------

grossrent$affordable_asian <- ifelse(incbyre_piv$Asian >= grossrent$grossrent, 1, 0)
grossrent$affordable_black <- ifelse(incbyre_piv$Black >= grossrent$grossrent, 1, 0)
grossrent$affordable_hispanic <- ifelse(incbyre_piv$`Hispanic/Latinx` >= grossrent$grossrent, 1, 0)
grossrent$affordable_white <- ifelse(incbyre_piv$White >= grossrent$grossrent, 1, 0)

#-------------- Summary of census tracts affordable to each RE category -------------

summarytbl <- data.frame(year)
summarytbl$affordable_asian <- sum(na.omit(grossrent$affordable_asian))/nrow(na.omit(grossrent))
summarytbl$affordable_black <- sum(na.omit(grossrent$affordable_black))/nrow(na.omit(grossrent))
summarytbl$affordable_hispanic <- sum(na.omit(grossrent$affordable_hispanic))/nrow(na.omit(grossrent))
summarytbl$affordable_white <- sum(na.omit(grossrent$affordable_white))/nrow(na.omit(grossrent))

#-------------- Write to Excel --------------
write.csv(grossrent, file_path, row.names=FALSE)