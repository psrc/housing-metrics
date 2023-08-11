# TITLE: Home Value & Rent 
# GEOGRAPHIES: Seattle MSA
# DATA SOURCE: Zillow
# DATE MODIFIED: 8.11.2023
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(stringr)
library(dplyr)

# assumptions
#  ZHVI: Zillow Home Value Index - All Homes (SFR & Condo) Time series, Smoothed, Seasonally-Adjusted
#  ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted

start_date <- "2015/01/06"
end_date <- "2023/30/06"
ZHVI_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_sa_month.csv?t=1691785225"

# ---------------- Pull in data ----------------
ZHVI_raw = read.csv(ZHVI_url)
ZORI_raw = read.csv(ZORI_url)

# ---------------- Clean data ----------------
ZHVI <- subset(ZHVI_raw, ZHVI_raw$RegionName == 'Seattle, WA')
ZORI <- subset(ZORI_raw, ZORI_raw$RegionName == 'Seattle, WA')

ZHVI$source <- "ZHVI"
ZORI$source <- "ZORI"

# ZHVI <- subset(ZHVI, select = c(RegionName,X2015.01.31))
# 
# all_data <- rbind(ZHVI,ZORI)
# 
# 
# 
# ifelse()
# names(ZHVI)=str_sub(names(ZHVI),2)
# names(ZORI)=str_sub(names(ZORI),2)

