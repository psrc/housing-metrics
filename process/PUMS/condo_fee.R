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
pums_raw <- get_psrc_pums(5, 2021, "h", c("TEN","CONP","PRACE","HINCP","YRBLT"))
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

# Create variables
pums_new_vars <- pums_raw %>% 
  mutate(income_bin=factor(case_when(HINCP < 50000 ~ "Under $50,000",
                                     HINCP < 75000 ~ "$50,000-$74,999",
                                     HINCP < 100000 ~ "$75,000-$99,999",
                                     HINCP < 150000 ~ "$100,000-$149,999",
                                     HINCP < 200000 ~ "$150,000-$199,999",
                                     HINCP >=200000 ~ "$200,000 or more",
                                     !is.na(HINCP) ~ "Else / Prefer not to answer"),
                           levels=c("Under $50,000",                                     
                                    "$50,000-$74,999",
                                    "$75,000-$99,999",
                                    "$100,000-$149,999",
                                    "$150,000-$199,999",
                                    "$200,000 or more",
                                    "Else / Prefer not to answer")),
         tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"other"),
                       levels=c("owner", "other")),
         PRACE=factor(
           case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                     grepl("^Black ", PRACE) ~"Black",
                     grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                     !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                       stringr::str_replace(" alone", "") %>%
                       stringr::str_replace(" Alone", ""))))

pums_new_vars <- pums_new_vars %>%
  filter(CONP > 0)

# Analyses --------------

condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure"), incl_na = FALSE, rr = TRUE) # analysis by region
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("TEN"), incl_na = FALSE, rr = TRUE) # analysis by mortgage status
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","COUNTY"), incl_na = FALSE, rr = TRUE) # analysis by county
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","PRACE"), incl_na = FALSE, rr = TRUE) # analysis by RE
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","income_bin"), incl_na = FALSE, rr = TRUE) # analysis by income
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","PRACE","income_bin"), incl_na = FALSE, rr = TRUE) # analysis by RE and income
condo_fee <- psrc_pums_median(pums_new_vars, "CONP", group_vars = c("tenure","YRBLT"), incl_na = FALSE, rr = TRUE) # analysis by year built

