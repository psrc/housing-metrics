# TITLE: Tenure by Race/Ethnicity and Income
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 2021 5YR ACS PUMS
# DATE MODIFIED: 5.02.2023
# AUTHOR: Eric Clute & Carol Naito

library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(tidyr)
library(openxlsx)
library(purrr)

years <- c(2010,2016,2021)


#-------------- Tenure by R/E Category --------------

tenure_re_func <- function(year){
  
  # Obtain the data
  setwd("C:/Users/eclute/Downloads")
  pums_raw <- get_psrc_pums(5, year, "h", c("TEN","PRACE","HINCP"))
  setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")
  
  # Create variables
  pums_new_vars <- pums_raw %>% 
    mutate(tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                         levels=c("owner", "renter")),
           PRACE=factor(
             case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                       grepl("^Black ", PRACE) ~"Black",
                       grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                       !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                         stringr::str_replace(" alone", "") %>%
                         stringr::str_replace(" Alone", ""))))
  
  # Analysis --------------
  tenure_re <- psrc_pums_count(pums_new_vars, group_vars = c("PRACE","tenure"),rr=TRUE)
  tenure_region <- psrc_pums_count(pums_new_vars, group_vars = c("tenure"),rr=TRUE)
  
  # Pivot table 1
  tenure_re_piv <- tenure_re %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'PRACE'),
                names_from = 'tenure',
                values_from = c('count', 'count_moe','reliability', 'share', 'share_moe'))
  
  tenure_re_piv <- tenure_re_piv[, c(1,2,12,13,15,16,14,17,3,4,5,6,7,8,9,10,11)]
  tenure_re_piv <- tenure_re_piv[tenure_re_piv$PRACE !='Total',]
  
  # Pivot table 2
  tenure_region_piv <- tenure_region %>%
    pivot_wider(id_cols = c( 'DATA_YEAR'),
                names_from = 'tenure',
                values_from = c('count', 'count_moe','reliability', 'share', 'share_moe'))
  
  tenure_region_piv$PRACE <- "Region Avg"
  
  # Combine together - adds regional average to table
  tenure_re_piv <- rbind(tenure_re_piv, tenure_region_piv)
  
}

# Run function -----------
tenure_re_piv_all <- map(years, ~tenure_re_func(.x)) %>%
  reduce(bind_rows)

# Graph ownership over time -----------
library(psrcplot)
library(ggplot2)

ownership_re <- interactive_line_chart(tenure_re_piv_all, "DATA_YEAR", "share_owner", fill = "PRACE",
                                           title="Change in Ownership by Race/Ethnicity",color="pognbgy_10")
ownership_re

#-------------- Group by Tenure (ownership), Income, R/E Category --------------

tenure_inc_func <- function(year){
  
  # Obtain the data
  setwd("C:/Users/eclute/Downloads")
  pums_raw <- get_psrc_pums(5, year, "h", c("TEN","PRACE","HINCP"))
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
           tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                         levels=c("owner", "renter")),
           PRACE=factor(
             case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                       grepl("^Black ", PRACE) ~"Black",
                       grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                       !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                         stringr::str_replace(" alone", "") %>%
                         stringr::str_replace(" Alone", ""))))
  
  # Analysis --------------
  tenure_inc_re <- psrc_pums_count(pums_new_vars, group_vars = c("income_bin","PRACE","tenure"),rr=TRUE)
  tenure_inc_re_region <- psrc_pums_count(pums_new_vars, group_vars = c("income_bin","tenure"),rr=TRUE)
  

  tenure_inc_re <- tenure_inc_re %>% filter(tenure=='owner')
  tenure_inc_re_region <- tenure_inc_re_region %>% filter(tenure=='owner')
  
  # Pivot table 1
  tenure_inc_re_piv <- tenure_inc_re %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'tenure', 'PRACE'),
                names_from = 'income_bin',
                values_from = c('count', 'count_moe', 'share', 'share_moe','reliability'))
  
  tenure_inc_re_piv<- tenure_inc_re_piv[, c(1,2,3,16,17,18,19,20,21,22,23,24,25,26,27,4,5,6,7,8,9,10,11,12,13,14,15,28,29,30,31,32,33)]
  
  # Pivot table 2
  tenure_inc_re_region_piv <- tenure_inc_re_region %>% 
    pivot_wider(id_cols = c( 'DATA_YEAR', 'tenure'),
                names_from = 'income_bin',
                values_from = c('count', 'count_moe', 'share', 'share_moe','reliability'))
  
  tenure_inc_re_region_piv$PRACE <- "Region Avg"
  
  # Combine together - adds regional average to table
  tenure_inc_re_piv <- rbind(tenure_inc_re_piv, tenure_inc_re_region_piv)
  
  # Limit to 4 largest RE groups (Asian, Black, Hispanic, White) - keep region avg
  tenure_inc_re_piv <- tenure_inc_re_piv %>% filter(!row_number() %in% c(1,5,6))

}

# Run function -----------
tenure_inc_re_all <- map(years, ~tenure_inc_func(.x)) %>%
  reduce(bind_rows)

#-------------- Write to Excel --------------
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "tenure by RE")
writeData(work_book, "tenure by RE", tenure_re_piv_all)
addWorksheet(work_book, sheetName = "ownership by RE & inc")
writeData(work_book, "ownership by RE & inc", tenure_inc_re_all)
saveWorkbook(work_book, file = "Tenure by RE and Income/r_output.xlsx", overwrite = TRUE)