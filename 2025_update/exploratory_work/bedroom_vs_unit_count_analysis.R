# TITLE: Counting Bedrooms Created in New Units over time
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 1YR ACS PUMS
# DATE MODIFIED: 01.17.2024
# AUTHOR: Eric Clute

library(psrccensus)
library(magrittr)
library(dplyr)
library(tidyr)

# Assumptions
year <- c(2022)

#-------------- Pull Data --------------
pums_raw <- get_psrc_pums(5, year, "h", c("BDSP", "YRBLT", "TEN"))

pums <- pums_raw %>% 
  mutate(
    unit_size = factor(case_when(
      BDSP == "0" ~ "studio",
      BDSP == "1" ~ "1bed",
      BDSP == "2" ~ "2bed",
      BDSP == "3" ~ "3bed",
      BDSP %in% c("4", "5", "6", "7") ~ "4bed+",
      TRUE ~ "other")),
    unit_size_rntr = factor(case_when(
      BDSP == "0" ~ "studio",
      BDSP == "1" ~ "1bed",
      BDSP == "2" ~ "2bed",
      BDSP %in% c("3", "4", "5", "6", "7") ~ "3bed+",
      TRUE ~ "other")),
    unit_size_ownr = factor(case_when(
      BDSP %in% c("0", "1") ~ "studio & 1bed",
      BDSP == "2" ~ "2bed",
      BDSP == "3" ~ "3bed",
      BDSP %in% c("4", "5", "6", "7") ~ "4bed+",
      TRUE ~ "other")),
    decade = factor(case_when(
      YRBLT == "1980 to 1989" ~ "1980s",
      YRBLT == "1990 to 1999" ~ "1990s",
      YRBLT == "2000 to 2009" ~ "2000s",
      YRBLT == "2010 to 2019" ~ "2010s",
      YRBLT %in% as.character(2020:year) ~ "2020s",
      TRUE ~ "1970s and earlier")),
    tenure=factor(case_when(
      TEN == "Owned free and clear" ~ "owner",
      TEN == "Owned with mortgage or loan (include home equity loans)" ~ "owner",
      TRUE ~ "renter")))

#-------------- Regional Analyses --------------
reg_current_yr_built <- psrc_pums_count(pums, group_vars = c("decade"),rr=TRUE)
reg_current_unit_size <- psrc_pums_count(pums, group_vars = c("unit_size"),rr=TRUE)
reg_current_tenure <- psrc_pums_count(pums, group_vars = c("tenure"),rr=TRUE)

#-------------- Regional Analysis - Unit Size by Decade ---------------
reg_unit_size <- psrc_pums_count(pums, decade, group_vars = c("decade", "unit_size"),rr=TRUE)

reg_unit_size_analysis <- reg_unit_size %>%
  filter(unit_size != "Total") %>%
  select(decade, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share)

#-------------- Regional Analysis - Unit Size by Decade and Tenure ---------------
reg_tenure_rntr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_rntr"),rr=TRUE)

reg_tenure_rntr_analysis <- reg_tenure_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share)

reg_tenure_ownr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_ownr"),rr=TRUE)

reg_tenure_ownr_analysis <- reg_tenure_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share)


#============== Testing Various Geographies ============== 

#-------------- County Analyses --------------
cnty_current_yr_built <- psrc_pums_count(pums, group_vars = c("COUNTY","decade"),rr=TRUE)
cnty_current_unit_size <- psrc_pums_count(pums, group_vars = c("COUNTY","unit_size"),rr=TRUE)
cnty_current_tenure <- psrc_pums_count(pums, group_vars = c("COUNTY","tenure"),rr=TRUE)

#-------------- County Analysis - Unit Size by Decade ---------------
cnty_unit_size <- psrc_pums_count(pums, decade, group_vars = c("COUNTY","decade", "unit_size"),rr=TRUE)

cnty_unit_size_analysis <- cnty_unit_size %>%
  filter(unit_size != "Total") %>%
  select(decade, COUNTY, unit_size, share) %>%
  pivot_wider(names_from = c(decade,COUNTY), values_from = share)

#-------------- County Analysis - Unit Size by Decade and Tenure ---------------
cnty_tenure_rntr <- psrc_pums_count(pums, decade, group_vars = c("COUNTY" ,"tenure", "decade", "unit_size_rntr"),rr=TRUE)

cnty_tenure_rntr_analysis <- cnty_tenure_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, COUNTY, unit_size_rntr, share) %>%
  pivot_wider(names_from = c(decade,COUNTY), values_from = share)

cnty_tenure_ownr <- psrc_pums_count(pums, decade, group_vars = c("COUNTY","tenure", "decade", "unit_size_ownr"),rr=TRUE)

cnty_tenure_ownr_analysis <- cnty_tenure_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade,COUNTY, unit_size_ownr, share) %>%
  pivot_wider(names_from = c(decade,COUNTY), values_from = share)