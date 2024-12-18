# TITLE: Counting Bedrooms Created in New Units over time
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 1YR ACS PUMS
# DATE MODIFIED: 12.18.2024
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

#-------------- Initial Analyses --------------
current_yr_built <- psrc_pums_count(pums, group_vars = c("decade"),rr=TRUE)
current_unit_size <- psrc_pums_count(pums, group_vars = c("unit_size"),rr=TRUE)
current_tenure <- psrc_pums_count(pums, group_vars = c("tenure"),rr=TRUE)

#-------------- Analysis - Unit Size by Decade ---------------
unit_size <- psrc_pums_count(pums, decade, group_vars = c("decade", "unit_size"),rr=TRUE)

analysis_unit_size <- unit_size %>%
  filter(unit_size != "Total") %>%
  select(decade, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share)

#-------------- Analysis - Unit Size by Decade and Tenure ---------------
tenure_rntr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_rntr"),rr=TRUE)

analysis_tenure_rntr <- tenure_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share)

tenure_ownr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_ownr"),rr=TRUE)

analysis_tenure_ownr <- tenure_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share)
