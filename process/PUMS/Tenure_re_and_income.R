# TITLE: Tenure by Race/Ethnicity and Income
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 2021 5YR ACS PUMS
# DATE MODIFIED: 3.27.2023
# AUTHOR: Eric Clute & Carol Naito

library(psrccensus)
library(magrittr)
library(dplyr)
library(srvyr)
library(openxlsx)

# Obtain the data
setwd("C:/Users/eclute/Downloads")
pums_raw <- get_psrc_pums(5, 2021, "h", c("TEN","PRACE","HINCP"))
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

#-------------- Tenure by Race Analysis --------------
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

#-------------- Group by Tenure, R/E Category --------------
tenure_re <- psrc_pums_count(pums_new_vars, group_vars = c("PRACE","tenure"))

# Pivot the table
tenure_re_piv <- tenure_re %>% 
  pivot_wider(id_cols = c( 'DATA_YEAR', 'PRACE'),
              names_from = 'tenure',
              values_from = c('count', 'count_moe', 'share', 'share_moe'))

tenure_re_piv <- tenure_re_piv[, c(1,2,9,10,12,13,11,14,3,4,5,6,7,8)]

#-------------- Group by Tenure (ownership), Income, R/E Category --------------
tenure_inc_re <- psrc_pums_count(pums_new_vars, group_vars = c("income_bin","PRACE","tenure"))
tenure_inc_re <- tenure_inc_re %>% filter(tenure=='owner')


# Pivot the table
tenure_inc_re_piv <- tenure_inc_re %>% 
  pivot_wider(id_cols = c( 'DATA_YEAR', 'tenure', 'PRACE'),
              names_from = 'income_bin',
              values_from = c('count', 'count_moe', 'share', 'share_moe'))

tenure_inc_re_piv<- tenure_inc_re_piv[, c(1,2,3,16,17,18,19,20,21,22,23,24,25,26,27,4,5,6,7,8,9,10,11,12,13,14,15)]

#-------------- Write to Excel --------------

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "tenure by RE")
writeData(work_book, "tenure by RE", tenure_re_piv)
addWorksheet(work_book, sheetName = "ownership by RE & inc")
writeData(work_book, "ownership by RE & inc", tenure_inc_re_piv)
saveWorkbook(work_book, file = "Tenure by RE and Income/r_output 2021 5YR.xlsx", overwrite = TRUE)