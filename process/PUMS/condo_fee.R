# TITLE: Condo Fee
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: 2021 5YR ACS PUMS
# DATE MODIFIED: 5.01.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(tidyr)
library(openxlsx)
library(purrr)

#-------------- Tenure by R/E Category --------------

# Obtain the data
setwd("C:/Users/eclute/Downloads")
pums_raw <- get_psrc_pums(5, 2021, "h", c("TEN","CONP"))
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

# Create variables
pums_new_vars <- pums_raw %>% 
  mutate(tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                       levels=c("owner")))
pums_new_vars <- pums_new_vars %>%
  filter(!is.na(tenure)) %>%
  filter(CONP > 0)
  

# Analysis --------------
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("COUNTY"), incl_na = TRUE, rr = TRUE)


#-------------- Write to Excel --------------

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "")
writeData(work_book, "o", condo_fee)
saveWorkbook(work_book, file = "New Housing Construction - Permitting Data/r_output-condo_fees.xlsx", overwrite = TRUE)