# TITLE: Counting Bedrooms Created in New Units over time
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 1YR ACS PUMS
# DATE MODIFIED: 01.28.2024
# AUTHOR: Eric Clute

library(psrccensus)
library(magrittr)
library(dplyr)
library(tidyr)
library(openxlsx)

# Assumptions
year <- c(2023)
unit_size_order <- c("studio","studio & 1bed", "1bed", "2bed", "3bed", "3bed+", "4bed+")
seattle_pumas <- c(23318, 23316, 23317, 23315, 23314, 23312, 23313) #2020 decennial boundaries
#PUMAs found here: https://psrc-psregcncl.hub.arcgis.com/datasets/5ad2945593df403eadbb6f6756ebb49c_0/explore?location=47.603732%2C-122.364195%2C10.92

export_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/Early Exploratory Work"
source_info <- c("2023 American Community Survey (ACS) PUMS, 5-Year. Crunched by Eric Clute.")

#-------------- Pull Data --------------
pums_raw <- get_psrc_pums(5, year, "h", c("BDSP", "YRBLT", "TEN", "PUMA"))

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
      TRUE ~ "renter")),
    kc_binary=factor(case_when(
      COUNTY == "King" ~ "King",
      TRUE ~ "Outside King")),
    sea_binary=factor(case_when(
      PUMA %in% seattle_pumas ~ "seattle",
      TRUE ~ "outside seattle"))
    )

#-------------- Regional Analyses --------------
reg_current_yr_built <- psrc_pums_count(pums, group_vars = c("decade"),rr=TRUE)
reg_current_unit_size <- psrc_pums_count(pums, group_vars = c("unit_size"),rr=TRUE)
reg_current_tenure <- psrc_pums_count(pums, group_vars = c("tenure"),rr=TRUE)

#-------------- Regional Analysis - Unit Size by Decade ---------------
reg_allunits <- psrc_pums_count(pums, decade, group_vars = c("decade", "unit_size"),rr=TRUE)

reg_allunits_analysis_reliability <- reg_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, unit_size, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(unit_size)

reg_allunits_share <- reg_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(unit_size)

reg_allunits_analysis <- bind_rows(reg_allunits_share, reg_allunits_analysis_reliability)
rm(reg_allunits, reg_allunits_analysis_reliability, reg_allunits_share)

#-------------- Regional Analysis - Unit Size by Decade and Tenure ---------------
reg_rntr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_rntr"),rr=TRUE)

reg_rntr_analysis_reliability <- reg_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, unit_size_rntr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(unit_size_rntr)

reg_rntr_analysis_share <- reg_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(unit_size_rntr)

reg_rntr_analysis_count <- reg_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, unit_size_rntr, count) %>%
  pivot_wider(names_from = decade, values_from = count) %>%
  mutate(across(everything(), as.character),
         unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(unit_size_rntr)

reg_rntr_analysis <- bind_rows(reg_rntr_analysis_count, reg_rntr_analysis_share, reg_rntr_analysis_reliability)
rm(reg_rntr, reg_rntr_analysis_count, reg_rntr_analysis_share, reg_rntr_analysis_reliability)

reg_ownr <- psrc_pums_count(pums, decade, group_vars = c("tenure", "decade", "unit_size_ownr"),rr=TRUE)

reg_ownr_analysis_reliability <- reg_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, unit_size_ownr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(unit_size_ownr)

reg_ownr_analysis_share <- reg_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(unit_size_ownr)

reg_ownr_analysis_count <- reg_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, unit_size_ownr, count) %>%
  pivot_wider(names_from = decade, values_from = count) %>%
  mutate(across(everything(), as.character),
         unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(unit_size_ownr)

reg_ownr_analysis <- bind_rows(reg_ownr_analysis_count, reg_ownr_analysis_share, reg_ownr_analysis_reliability)
rm(reg_ownr,reg_ownr_analysis_count, reg_ownr_analysis_share, reg_ownr_analysis_reliability)

#============== Testing Various Geographies ============== 

#-------------- County Analyses --------------
cnty_current_yr_built <- psrc_pums_count(pums, group_vars = c("COUNTY","decade"),rr=TRUE)
cnty_current_unit_size <- psrc_pums_count(pums, group_vars = c("COUNTY","unit_size"),rr=TRUE)
cnty_current_tenure <- psrc_pums_count(pums, group_vars = c("COUNTY","tenure"),rr=TRUE)

#-------------- County Analysis - Unit Size by Decade ---------------
cnty_allunits <- psrc_pums_count(pums, decade, group_vars = c("COUNTY","decade", "unit_size"),rr=TRUE)

cnty_allunits_analysis_reliability <- cnty_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, COUNTY, unit_size, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(COUNTY,unit_size)

cnty_allunits_share <- cnty_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, COUNTY, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(COUNTY,unit_size)

cnty_allunits_analysis <- bind_rows(cnty_allunits_share, cnty_allunits_analysis_reliability)
cnty_allunits_analysis <- cnty_allunits_analysis %>% arrange(COUNTY)
rm(cnty_allunits, cnty_allunits_analysis_reliability, cnty_allunits_share)

#-------------- County Analysis - Unit Size by Decade and Tenure ---------------
cnty_rntr <- psrc_pums_count(pums, decade, group_vars = c("COUNTY" ,"tenure", "decade", "unit_size_rntr"),rr=TRUE)

cnty_rntr_analysis_reliability <- cnty_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, COUNTY, unit_size_rntr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(COUNTY, unit_size_rntr)

cnty_rntr_analysis_share <- cnty_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, COUNTY, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(COUNTY, unit_size_rntr)

cnty_rntr_analysis <- bind_rows(cnty_rntr_analysis_share,cnty_rntr_analysis_reliability)
cnty_rntr_analysis <- cnty_rntr_analysis %>% arrange(COUNTY)
rm(cnty_rntr, cnty_rntr_analysis_reliability, cnty_rntr_analysis_share)

cnty_ownr <- psrc_pums_count(pums, decade, group_vars = c("COUNTY","tenure", "decade", "unit_size_ownr"),rr=TRUE)

cnty_ownr_analysis_reliability <- cnty_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, COUNTY, unit_size_ownr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(COUNTY, unit_size_ownr)

cnty_ownr_analysis_share <- cnty_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, COUNTY, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(COUNTY, unit_size_ownr)

cnty_ownr_analysis <- bind_rows(cnty_ownr_analysis_share,cnty_ownr_analysis_reliability)
cnty_ownr_analysis <- cnty_ownr_analysis %>% arrange(COUNTY)
rm(cnty_ownr, cnty_ownr_analysis_reliability, cnty_ownr_analysis_share)

#==============

#-------------- King County vs Outside King County Analyses --------------
kc_current_yr_built <- psrc_pums_count(pums, group_vars = c("kc_binary","decade"),rr=TRUE)
kc_current_unit_size <- psrc_pums_count(pums, group_vars = c("kc_binary","unit_size"),rr=TRUE)
kc_current_tenure <- psrc_pums_count(pums, group_vars = c("kc_binary","tenure"),rr=TRUE)

#-------------- KC vs Outside KC Analyses - Unit Size by Decade ---------------
kc_allunits <- psrc_pums_count(pums, decade, group_vars = c("kc_binary","decade", "unit_size"),rr=TRUE)

kc_allunits_analysis_reliability <- kc_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, kc_binary, unit_size, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(kc_binary,unit_size)

kc_allunits_analysis_share <- kc_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, kc_binary, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(kc_binary,unit_size)

kc_allunits_analysis <- bind_rows(kc_allunits_analysis_share, kc_allunits_analysis_reliability)
kc_allunits_analysis <- kc_allunits_analysis %>% arrange(kc_binary)
rm(kc_allunits, kc_allunits_analysis_reliability, kc_allunits_analysis_share)

#-------------- KC vs Outside KC Analyses - Unit Size by Decade and Tenure ---------------
kc_rntr <- psrc_pums_count(pums, decade, group_vars = c("kc_binary" ,"tenure", "decade", "unit_size_rntr"),rr=TRUE)

kc_rntr_analysis_reliability <- kc_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, kc_binary, unit_size_rntr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(kc_binary, unit_size_rntr)

kc_rntr_analysis_share <- kc_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, kc_binary, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(kc_binary, unit_size_rntr)

kc_rntr_analysis <- bind_rows(kc_rntr_analysis_share,kc_rntr_analysis_reliability)
kc_rntr_analysis <- kc_rntr_analysis %>% arrange(kc_binary)
rm(kc_rntr, kc_rntr_analysis_reliability, kc_rntr_analysis_share)

kc_ownr <- psrc_pums_count(pums, decade, group_vars = c("kc_binary","tenure", "decade", "unit_size_ownr"),rr=TRUE)

kc_ownr_analysis_reliability <- kc_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, kc_binary, unit_size_ownr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(kc_binary, unit_size_ownr)

kc_ownr_analysis_share <- kc_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, kc_binary, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(kc_binary, unit_size_ownr)

kc_ownr_analysis <- bind_rows(kc_ownr_analysis_share,kc_ownr_analysis_reliability)
kc_ownr_analysis <- kc_ownr_analysis %>% arrange(kc_binary)
rm(kc_ownr, kc_ownr_analysis_reliability, kc_ownr_analysis_share)

#==============

#-------------- Seattle vs Outside Seattle Analyses --------------
sea_current_yr_built <- psrc_pums_count(pums, group_vars = c("sea_binary","decade"),rr=TRUE)
sea_current_unit_size <- psrc_pums_count(pums, group_vars = c("sea_binary","unit_size"),rr=TRUE)
sea_current_tenure <- psrc_pums_count(pums, group_vars = c("sea_binary","tenure"),rr=TRUE)

#-------------- Sea vs Outside Sea Analyses - Unit Size by Decade ---------------
sea_allunits <- psrc_pums_count(pums, decade, group_vars = c("sea_binary","decade", "unit_size"),rr=TRUE)

sea_allunits_analysis_reliability <- sea_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, sea_binary, unit_size, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(sea_binary,unit_size)

sea_allunits_analysis_share <- sea_allunits %>%
  filter(unit_size != "Total") %>%
  select(decade, sea_binary, unit_size, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size = factor(unit_size, levels = unit_size_order)) %>%
  arrange(sea_binary,unit_size)

sea_allunits_analysis <- bind_rows(sea_allunits_analysis_share, sea_allunits_analysis_reliability)
sea_allunits_analysis <- sea_allunits_analysis %>% arrange(sea_binary)
rm(sea_allunits, sea_allunits_analysis_reliability, sea_allunits_analysis_share)

#-------------- Sea vs Outside Sea Analyses - Unit Size by Decade and Tenure ---------------
sea_rntr <- psrc_pums_count(pums, decade, group_vars = c("sea_binary" ,"tenure", "decade", "unit_size_rntr"),rr=TRUE)

sea_rntr_analysis_reliability <- sea_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, sea_binary, unit_size_rntr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(sea_binary, unit_size_rntr)

sea_rntr_analysis_share <- sea_rntr %>%
  filter(tenure == "renter", unit_size_rntr != "Total") %>%
  select(decade, sea_binary, unit_size_rntr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_rntr = factor(unit_size_rntr, levels = unit_size_order)) %>%
  arrange(sea_binary, unit_size_rntr)

sea_rntr_analysis <- bind_rows(sea_rntr_analysis_share,sea_rntr_analysis_reliability)
sea_rntr_analysis <- sea_rntr_analysis %>% arrange(sea_binary)
rm(sea_rntr, sea_rntr_analysis_reliability, sea_rntr_analysis_share)

sea_ownr <- psrc_pums_count(pums, decade, group_vars = c("sea_binary","tenure", "decade", "unit_size_ownr"),rr=TRUE)

sea_ownr_analysis_reliability <- sea_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, sea_binary, unit_size_ownr, reliability) %>%
  pivot_wider(names_from = decade, values_from = reliability) %>%
  mutate(unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(sea_binary, unit_size_ownr)

sea_ownr_analysis_share <- sea_ownr %>%
  filter(tenure == "owner", unit_size_ownr != "Total") %>%
  select(decade, sea_binary, unit_size_ownr, share) %>%
  pivot_wider(names_from = decade, values_from = share) %>%
  mutate(across(everything(), as.character),
         unit_size_ownr = factor(unit_size_ownr, levels = unit_size_order)) %>%
  arrange(sea_binary, unit_size_ownr)

sea_ownr_analysis <- bind_rows(sea_ownr_analysis_share,sea_ownr_analysis_reliability)
sea_ownr_analysis <- sea_ownr_analysis %>% arrange(sea_binary)
rm(sea_ownr, sea_ownr_analysis_reliability, sea_ownr_analysis_share)

# Cleanup and export ---------------------
export_file <- paste0(export_path, "/unit_count_raw.xlsx")
work_book <- createWorkbook()

addWorksheet(work_book, sheetName = "reg_allunits_analysis")
writeData(work_book, sheet = "reg_allunits_analysis", reg_allunits_analysis)
writeData(work_book, sheet = "reg_allunits_analysis", x = data.frame(source_info), startRow = nrow(reg_allunits_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "reg_ownr_analysis")
writeData(work_book, sheet = "reg_ownr_analysis", reg_ownr_analysis)
writeData(work_book, sheet = "reg_ownr_analysis", x = data.frame(source_info), startRow = nrow(reg_ownr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "reg_rntr_analysis")
writeData(work_book, sheet = "reg_rntr_analysis", reg_rntr_analysis)
writeData(work_book, sheet = "reg_rntr_analysis", x = data.frame(source_info), startRow = nrow(reg_rntr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "cnty_allunits_analysis")
writeData(work_book, sheet = "cnty_allunits_analysis", cnty_allunits_analysis)
writeData(work_book, sheet = "cnty_allunits_analysis", x = data.frame(source_info), startRow = nrow(cnty_allunits_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "cnty_ownr_analysis")
writeData(work_book, sheet = "cnty_ownr_analysis", cnty_ownr_analysis)
writeData(work_book, sheet = "cnty_ownr_analysis", x = data.frame(source_info), startRow = nrow(cnty_ownr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "cnty_rntr_analysis")
writeData(work_book, sheet = "cnty_rntr_analysis", cnty_rntr_analysis)
writeData(work_book, sheet = "cnty_rntr_analysis", x = data.frame(source_info), startRow = nrow(cnty_rntr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "kc_allunits_analysis")
writeData(work_book, sheet = "kc_allunits_analysis", kc_allunits_analysis)
writeData(work_book, sheet = "kc_allunits_analysis", x = data.frame(source_info), startRow = nrow(kc_allunits_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "kc_ownr_analysis")
writeData(work_book, sheet = "kc_ownr_analysis", kc_ownr_analysis)
writeData(work_book, sheet = "kc_ownr_analysis", x = data.frame(source_info), startRow = nrow(kc_ownr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "kc_rntr_analysis")
writeData(work_book, sheet = "kc_rntr_analysis", kc_rntr_analysis)
writeData(work_book, sheet = "kc_rntr_analysis", x = data.frame(source_info), startRow = nrow(kc_rntr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "sea_allunits_analysis")
writeData(work_book, sheet = "sea_allunits_analysis", sea_allunits_analysis)
writeData(work_book, sheet = "sea_allunits_analysis", x = data.frame(source_info), startRow = nrow(sea_allunits_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "sea_ownr_analysis")
writeData(work_book, sheet = "sea_ownr_analysis", sea_ownr_analysis)
writeData(work_book, sheet = "sea_ownr_analysis", x = data.frame(source_info), startRow = nrow(sea_ownr_analysis) + 3, startCol = 1)

addWorksheet(work_book, sheetName = "sea_rntr_analysis")
writeData(work_book, sheet = "sea_rntr_analysis", sea_rntr_analysis)
writeData(work_book, sheet = "sea_rntr_analysis", x = data.frame(source_info), startRow = nrow(sea_rntr_analysis) + 3, startCol = 1)

saveWorkbook(work_book, file = export_file, overwrite = TRUE)
