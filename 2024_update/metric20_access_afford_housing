# TITLE: Affordable Rental Housing by Tract - for all renters
# GEOGRAPHIES: PSRC Region & Census Tract
# DATA SOURCE: 5YR ACS Data
# LAST EDITED: 6.27.2024
# AUTHOR: Kristin Mitchell

library(psrccensus)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(srvyr)
library(fredr)

year <- (2022)
inflation_year <- (2022)
#file_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric20_renter_hh_income_affordability/"

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
# To use the fredr package and pce_deflator, go to https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html 
# to get more info on how to obtain an API key
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

# Create/modify variables for r/e
pums <- pums_raw %>% mutate(PRACE=factor(
  case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
            grepl("^Black ", PRACE) ~"Black",
            grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
            !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
              stringr::str_replace(" alone", "") %>%
              stringr::str_replace(" Alone", ""))))

incbyre <- psrc_pums_median(pums, "HINCP2022", group_vars = c("DATA_YEAR","TEN", "PRACE"),rr=TRUE)
incbyre <- filter(incbyre, TEN == "Rented")

incbyre <- incbyre %>% rename("HINCP_median" = "HINCP2022_median")
incbyre <- incbyre %>% rename("HINCP_median_moe" = "HINCP2022_median_moe")

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
#write.csv(grossrent, "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric20_renter_hh_income_affordability/rentersbyre.csv", row.names=FALSE)

#------------ Median Renter HH Income for All Renters ------------
pums_raw_all <- get_psrc_pums(5,year,"h",c("TEN","HINCP","GRNTP"))
pums_raw_all <- real_dollars(pums_raw, inflation_year)

# Create/modify variables
pums <- pums_raw_all

incall <- psrc_pums_median(pums, "HINCP", group_vars = c("DATA_YEAR","TEN"),rr=TRUE)
incall <- filter(incall, TEN == "Rented")

incall <- incall %>% rename("HINCP_median" = "HINCP_median")
incall <- incall %>% rename("HINCP_median_moe" = "HINCP_median_moe")

# Create new fields, calculate cost max rent, moe upper/lower
incall$maxmonthlyrent <- incall$HINCP_median/12*0.3
incall$moeupperbound <- incall$HINCP_median + incall$HINCP_median_moe
incall$moelowerbound <- incall$HINCP_median - incall$HINCP_median_moe

# Create reference table
incall_piv <- incall
incall_piv <- incall_piv %>% pivot_wider(names_from = COUNTY, values_from = maxmonthlyrent)

#-------------- Indicate which tracts are affordable --------------
grossrent$affordable <- ifelse(incall_piv$Region >= grossrent$grossrent, 1,0)

#-------------- Summary of census tracts affordable -------------
summarytbl <- data.frame(year)
summarytbl$affordable <- sum(na.omit(grossrent$affordable))/nrow(na.omit(grossrent))

#-------------- Region-wide Median Gross Rent --------------
pums_rent <- pums_raw_all
medianrent_region <- psrc_pums_median(pums_rent, "GRNTP", group_vars = c("DATA_YEAR"),rr=TRUE)

grossrentall <- grossrent

#-------------- Write to Excel --------------
#write.csv(grossrentall, "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric20_renter_hh_income_affordability/allrenters.csv", row.names=FALSE)
