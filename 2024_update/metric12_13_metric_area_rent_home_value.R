# TITLE: Home Value & Rent 
# GEOGRAPHIES: Seattle MSA
# DATA SOURCE: Zillow, ACS
# DATE MODIFIED: 8.19.2024
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)
library(psrccensus)
library(tidycensus)
library(purrr)

# assumptions
#  ZHVI: Zillow Home Value Index - All Homes (SFR & Condo) Time series, Smoothed, Seasonally-Adjusted
#  ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted

export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric12-13_metro_area_rent_home_value"
ZHVI_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_sa_month.csv?t=1711667054"

years <- c(2015,2016,2017,2018,2019,2021,2022)

# ---------------- Functions ------------------
pull_data <- function(years){
  # returns a list of get_psrc_pums outputs, one for each year
  lapply(years, function(x) get_psrc_pums(1, x, "h",c("HINCP")))
 }

inc_clean_func <- function(hh_inc){
  hh_inc_calc <- psrc_pums_median(hh_inc, "HINCP", group_vars = "MSA", rr=TRUE)
}

# ---------------- HH Income data -----------------
# I know this is messy code - I couldn't figure out how to have the MSA column be created within the function. Could be cleaned up!
# pull the data
hh_inc <- pull_data(years)

hh_inc[[1]][["variables"]] <- hh_inc[[1]][["variables"]] %>%
  mutate(hh_inc[[1]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[2]][["variables"]] <- hh_inc[[2]][["variables"]] %>%
  mutate(hh_inc[[2]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[3]][["variables"]] <- hh_inc[[3]][["variables"]] %>%
  mutate(hh_inc[[3]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[4]][["variables"]] <- hh_inc[[4]][["variables"]] %>%
  mutate(hh_inc[[4]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[5]][["variables"]] <- hh_inc[[5]][["variables"]] %>%
  mutate(hh_inc[[5]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[6]][["variables"]] <- hh_inc[[6]][["variables"]] %>%
  mutate(hh_inc[[6]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

hh_inc[[7]][["variables"]] <- hh_inc[[7]][["variables"]] %>%
  mutate(hh_inc[[7]][["variables"]], MSA=dplyr::case_when(COUNTY=="Kitsap" ~NA_character_, !is.na(COUNTY) ~ "Seattle Tacoma Bellevue MSA"))

# Summarize by income
hh_inc_all <- map(hh_inc, ~inc_clean_func(.x)) %>%
  reduce(bind_rows) %>%
  filter(MSA == "Seattle Tacoma Bellevue MSA")


# ---------------- Pull in Zillow data ----------------
ZHVI_raw = read.csv(ZHVI_url)
ZORI_raw = read.csv(ZORI_url)

# ---------------- Clean data ----------------
ZHVI <- subset(ZHVI_raw, ZHVI_raw$RegionName == 'Seattle, WA')
ZORI <- subset(ZORI_raw, ZORI_raw$RegionName == 'Seattle, WA')

ZHVI$source <- "ZHVI"
ZORI$source <- "ZORI"

common <- intersect(colnames(ZHVI), colnames(ZORI))
all_data <- rbind(ZHVI[common], ZORI[common])

all_data %<>%
  relocate(source, .before = RegionID)

colnames(all_data)<-gsub("X","",colnames(all_data))

# Export
write.csv(all_data, file = file.path(export_path, "metric12_13_raw.csv"), row.names = FALSE)
