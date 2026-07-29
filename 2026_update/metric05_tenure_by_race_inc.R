# TITLE: Tenure by Race/Ethnicity and Income
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS PUMS
# DATE CREATED: 5.5.2026
# AUTHOR: Eric Clute

library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(tidyr)
library(openxlsx)
library(purrr)
library(stringr)

options(timeout = 600, download.file.method = "wininet")

# Assumptions
years <- c(2010,2016,2024)
setwd("J:/Projects/V2050/Housing/Monitoring/2026Update/data/metric05_tenure_by_race_inc")

#-------------- Tenure by R/E Category --------------

tenure_re_func <- function(year){
  
  # Obtain the data
  pums_raw_reg <- get_psrc_pums(5, year, "h", c("TEN","PRACE","HINCP","AGEP"))
  
  # Create variables
  pums_new_vars_reg <- pums_raw_reg %>% 
    mutate(tenure=factor(case_when(TEN=="Owned free and clear"| TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                         levels=c("owner", "renter")),
           PRACE=factor(
             case_when(grepl("^Some", PRACE) ~"Another Racial Identity",
                       grepl("^Two", PRACE) ~"Multiracial",
                       grepl("^Black ", PRACE) ~"Black",
                       grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                       !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                         stringr::str_replace(" alone", "") %>%
                         stringr::str_replace(" Alone", "")))) 
  
  # Analysis --------------
  tenure_re <- psrc_pums_count(pums_new_vars_reg, group_vars = c("PRACE","tenure"),rr=TRUE)
  tenure_region <- psrc_pums_count(pums_new_vars_reg, group_vars = c("tenure"),rr=TRUE)
  
  tenure_re_med_age <- psrc_pums_median(pums_new_vars_reg, stat_var = "AGEP", group_vars = c("PRACE", "tenure"))
  tenure_region_med_age <- psrc_pums_median(pums_new_vars_reg, stat_var = "AGEP", group_vars = c("tenure"))
  
  #Join age data with race/income
  tenure_re <- tenure_re %>%
    left_join(tenure_re_med_age %>%
                select(DATA_YEAR, COUNTY, PRACE, tenure, AGEP_median, AGEP_median_moe),
              by = c("DATA_YEAR", "COUNTY", "PRACE", "tenure"))
  
  tenure_region <- tenure_region %>%
    left_join(tenure_region_med_age %>%
                select(DATA_YEAR, COUNTY, tenure, AGEP_median, AGEP_median_moe),
              by = c("DATA_YEAR", "COUNTY", "tenure"))
  
  # Pivot table 1
  tenure_re_piv <- tenure_re %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'PRACE'),
                names_from = 'tenure',
                values_from = c('count', 'count_moe','reliability', 'share', 'share_moe', 'AGEP_median', 'AGEP_median_moe'))
  
  tenure_re_piv <- tenure_re_piv |> select(DATA_YEAR,
                                            PRACE,
                                            share_owner, share_renter, share_moe_owner, share_moe_renter,
                                            share_Total, share_moe_Total,
                                            count_owner, count_renter, count_Total,
                                            count_moe_owner, count_moe_renter, count_moe_Total,
                                            reliability_owner, reliability_renter, reliability_Total,
                                            AGEP_median_owner, AGEP_median_renter, AGEP_median_Total,
                                            AGEP_median_moe_owner, AGEP_median_moe_renter, AGEP_median_moe_Total) |>
    filter(PRACE != "Total")
  
  # Pivot table 2
  tenure_region_piv <- tenure_region %>%
    pivot_wider(id_cols = c( 'DATA_YEAR'),
                names_from = 'tenure',
                values_from = c('count', 'count_moe','reliability', 'share', 'share_moe', 'AGEP_median', 'AGEP_median_moe'))
  
  tenure_region_piv$PRACE <- "Region Avg"
  
  # Combine together - adds regional average to table
  tenure_re_piv <- rbind(tenure_re_piv, tenure_region_piv)
  
}

#-------------- Group by Tenure (ownership), Income, R/E Category --------------

tenure_inc_func <- function(year){
  
  # Obtain the data
  pums_raw_re <- get_psrc_pums(5, year, "h", c("TEN","PRACE","HINCP","AGEP"))

  # Create variables
  pums_new_vars_re <- pums_raw_re %>% 
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
           tenure=factor(case_when(TEN=="Owned free and clear" | TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                         levels=c("owner", "renter")),
           PRACE=factor(
             case_when(grepl("^Some", PRACE) ~"Another Racial Identity",
                       grepl("^Two", PRACE) ~"Multiracial",
                       grepl("^Black ", PRACE) ~"Black",
                       grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                       !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                         stringr::str_replace(" alone", "") %>%
                         stringr::str_replace(" Alone", ""))))
  
  # Analysis --------------
  tenure_inc_re <- psrc_pums_count(pums_new_vars_re, group_vars = c("income_bin","PRACE","tenure"),rr=TRUE)
  tenure_inc_re_region <- psrc_pums_count(pums_new_vars_re, group_vars = c("income_bin","tenure"),rr=TRUE)
  
  tenure_inc_re_med_age <- psrc_pums_median(pums_new_vars_re, stat_var = "AGEP", group_vars = c("income_bin", "PRACE", "tenure"))
  tenure_inc_re_region_med_age <- psrc_pums_median(pums_new_vars_re, stat_var = "AGEP", group_vars = c("income_bin", "tenure"))

  tenure_inc_re <- tenure_inc_re %>% filter(tenure=='owner')
  tenure_inc_re_region <- tenure_inc_re_region %>% filter(tenure=='owner')
 
  #Join age data with race/income
  tenure_inc_re <- tenure_inc_re %>%
    left_join(tenure_inc_re_med_age %>%
                select(DATA_YEAR, COUNTY, income_bin, PRACE, tenure, AGEP_median, AGEP_median_moe),
              by = c("DATA_YEAR", "COUNTY", "income_bin", "PRACE", "tenure"))
  
  tenure_inc_re_region <- tenure_inc_re_region %>%
    left_join(tenure_inc_re_region_med_age %>%
                select(DATA_YEAR, COUNTY, income_bin, tenure, AGEP_median, AGEP_median_moe),
              by = c("DATA_YEAR","income_bin", "COUNTY", "tenure"))
   
  # Pivot table 1
  tenure_inc_re_piv <- tenure_inc_re %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'tenure', 'PRACE'),
                names_from = 'income_bin',
                values_from = c('count', 'count_moe', 'share', 'share_moe','reliability', 'AGEP_median', 'AGEP_median_moe'))
  
  # Pivot table 2
  tenure_inc_re_region_piv <- tenure_inc_re_region %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'tenure'),
                names_from = 'income_bin',
                values_from = c('count', 'count_moe', 'share', 'share_moe','reliability', 'AGEP_median', 'AGEP_median_moe'))
  
  tenure_inc_re_region_piv$PRACE <- "Region Avg"
  
  # Combine together - adds regional average to table
  tenure_inc_re_piv <- rbind(tenure_inc_re_piv, tenure_inc_re_region_piv)

}

# Run functions -----------

tenure_re_piv_all <- map(years, ~tenure_re_func(.x)) %>%
  reduce(bind_rows)

tenure_inc_re_all <- map(years, ~tenure_inc_func(.x)) %>%
  reduce(bind_rows)

# Graph ownership over time -----------
library(psrcplot)
library(ggplot2)

ownership_re <- interactive_line_chart(tenure_re_piv_all, "DATA_YEAR", "share_owner", fill = "PRACE",
                                       title="Change in Ownership by Race/Ethnicity",color="pognbgy_10")
ownership_re

# Format small tables to be exported ------------------------
tenure_re_smalltbl <- pivot_wider(tenure_re_piv_all,id_cols = PRACE, names_from = DATA_YEAR, values_from = c(share_owner, share_moe_owner))

tenure_inc_re_smalltbl <- tenure_inc_re_all %>% 
  select(DATA_YEAR,PRACE, `share_Under $50,000`, `share_$50,000-$74,999`, `share_$75,000-$99,999`, `share_$100,000-$149,999`, `share_$150,000-$199,999`, `share_$200,000 or more`,
         `share_moe_Under $50,000`, `share_moe_$50,000-$74,999`, `share_moe_$75,000-$99,999`, `share_moe_$75,000-$99,999`, `share_moe_$100,000-$149,999`, `share_moe_$150,000-$199,999`, `share_moe_$200,000 or more`)

#-------------- Write to Excel --------------
work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "tenure by RE")
writeData(work_book, "tenure by RE", tenure_re_smalltbl)
addWorksheet(work_book, sheetName = "ownership by RE & inc")
writeData(work_book, "ownership by RE & inc", tenure_inc_re_smalltbl)
saveWorkbook(work_book, file = "metric05_raw.xlsx", overwrite = TRUE)