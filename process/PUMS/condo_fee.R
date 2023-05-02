# TITLE: Condo Fee
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 2021 5YR ACS PUMS
# DATE MODIFIED: 5.02.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(tidyr)
library(openxlsx)
library(purrr)

#-------------- Pull PUMS data, create variables, filter for condo fees > $0 --------------

# Obtain the data
setwd("C:/Users/eclute/Downloads")
pums_raw <- get_psrc_pums(5, 2021, "h", c("TEN","CONP","BLD"))
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

# Create variables
pums_new_vars <- pums_raw %>% 
  mutate(tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"other"),
                       levels=c("owner", "other"))) # create new variable that creates bins from the BLD variable (SF detached, SF attached (townhouses), Multi-Family, MH/Other)

pums_new_vars <- pums_new_vars %>%
  filter(CONP > 0)

# Analyses --------------

condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure"), incl_na = FALSE, rr = TRUE) # analysis by region
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("TEN"), incl_na = FALSE, rr = TRUE) # analysis by mortgage status
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","COUNTY"), incl_na = FALSE, rr = TRUE) # analysis by county
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","BLD"), incl_na = FALSE, rr = TRUE) # analysis by RE

