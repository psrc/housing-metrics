# TITLE: Home Value & Rent 
# GEOGRAPHIES: Seattle MSA
# DATA SOURCE: Zillow, ACS
# DATE MODIFIED: 8.26.2024
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)
library(psrccensus)
library(tidycensus)
library(purrr)
library(readxl)
library(psrchousing)

# assumptions
#  ZHVI: Zillow Home Value Index - All Homes (SFR & Condo) Time series, Smoothed, Seasonally-Adjusted
#  ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted

export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric12-13_metro_area_rent_home_value"
ZHVI_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_sa_month.csv?t=1711667054"

ofm_inc_url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/economy/median_household_income_estimates.xlsx"
ofm_inc_file <- "median_household_income_estimates.xlsx"

years <- c(2015,2016,2017,2018,2019,2021,2022)
counties <- c("King", "Pierce", "Snohomish")

# ---------------- Functions ------------------
pull_data <- function(years){
  # returns a list of get_psrc_pums outputs, one for each year
  lapply(years, function(x) get_psrc_pums(1, x, "h",c("HINCP")))
 }

inc_clean_func <- function(hh_inc){
  hh_inc_calc <- psrc_pums_median(hh_inc, "HINCP", group_vars = "MSA", rr=TRUE)
}

# Function to get columns that start with a number
get_numeric_cols <- function(df) {
  colnames(df)[str_detect(colnames(df), "^[0-9]")]  # Regular expression to match column names starting with a digit
}

# ---------------- ACS HH Income data -----------------
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

# ---------------- OFM Income & Pop Data ----------------
# Pull data
download.file(ofm_inc_url, ofm_inc_file, mode = "wb")
ofm_pop_raw <- ofm_county_population_data()

# Clean OFM income data
ofm_inc_raw <- read_excel(ofm_inc_file, skip = 3, col_names = TRUE, n_max = 41)
ofm_inc <- ofm_inc_raw

colnames(ofm_inc)[1] <- "County"
ofm_inc <- ofm_inc[-1, ] # Remove the now redundant header row
ofm_inc <- ofm_inc %>% filter(County %in% counties)

ofm_inc <- ofm_inc %>%
  pivot_longer(
    cols = get_numeric_cols(ofm_inc),  # Use the function to get columns that start with a number
    names_to = "Year",             # New column name for years
    values_to = "Median Income"    # New column name for median income
  ) %>% mutate(key = paste(Year, County, sep = "-")) # Create the key field by combining Year and County

# Clean OFM population data
ofm_pop <- ofm_pop_raw %>% filter(geography %in% counties) %>% select(year, geography, population) %>%
                           mutate(key = paste(year, geography, sep = "-")) # Create the key field by combining Year and County

# Join together OFM datasets
ofm_combined <- left_join(ofm_inc, ofm_pop, by = "key")
ofm_combined <- ofm_combined %>% select (c(County, Year, `Median Income`, population))

# Calculate weighted average
ofm_analysis <- ofm_combined %>%
  pivot_wider(names_from = County, values_from = c(`Median Income`, population)) %>%
  mutate(population_msa = population_King + population_Pierce + population_Snohomish,
         prct_king = population_King / population_msa,
         prct_pierce = population_Pierce / population_msa,
         prct_snohomish = population_Snohomish / population_msa,
         
         weight_king = `Median Income_King` * prct_king,
         weight_pierce = `Median Income_Pierce` * prct_pierce,
         weight_snohomish = `Median Income_Snohomish` * prct_snohomish,
         
         msa_weighted_inc = weight_king + weight_pierce + weight_snohomish)

# ---------------- Zillow data ----------------
ZHVI_raw = read.csv(ZHVI_url)
ZORI_raw = read.csv(ZORI_url)

# Clean Zillow data
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
