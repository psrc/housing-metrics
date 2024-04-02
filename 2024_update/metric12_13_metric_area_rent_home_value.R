# TITLE: Home Value & Rent 
# GEOGRAPHIES: Seattle MSA
# DATA SOURCE: Zillow
# DATE MODIFIED: 3.28.2024
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)

# assumptions
#  ZHVI: Zillow Home Value Index - All Homes (SFR & Condo) Time series, Smoothed, Seasonally-Adjusted
#  ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted

export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric12_metro_area_rent"

ZHVI_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_sa_month.csv?t=1711667054"

# ---------------- Pull in data ----------------
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
#write.csv(all_data, file = file.path(export_path, "metric12_13_raw.csv"), row.names = FALSE)
