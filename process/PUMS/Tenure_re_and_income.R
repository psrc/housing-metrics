# TITLE: Tenure by Race/Ethnicity and Income
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 2021 5YR ACS PUMS
# DATE MODIFIED: 3.22.2023
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
pums_new_vars <- pums20_5year %>% 
  
  mutate(POCYN=factor(case_when(PRACE=="White alone" ~"Non-POC", !is.na(PRACE) ~"POC"), 
                      levels=c("POC","Non-POC")),
         income_bin=factor(case_when(HINCP < 35000 ~ "Under $35,000",
                                     HINCP < 50000 ~ "$35,000-$49,999",
                                     HINCP < 75000 ~ "$50,000-$74,999",
                                     HINCP < 100000 ~ "$75,000-$99,999",
                                     HINCP < 150000 ~ "$100,000-$149,999",
                                     HINCP < 200000 ~ "$150,000-$199,999",
                                     HINCP >=200000 ~ "$200,000 or more",
                                     !is.na(HINCP) ~ "Else / Prefer not to answer"),
                           levels=c("Under $35,000",
                                    "$35,000-$49,999",                                     
                                    "$50,000-$74,999",
                                    "$75,000-$99,999",
                                    "$100,000-$149,999",
                                    "$150,000-$199,999",
                                    "$200,000 or more",
                                    "Else / Prefer not to answer")),
         tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", !is.na(TEN) ~"renter"),
                       levels=c("owner", "renter")))


#-------------- Group by R/E Category, Income, Tenure - keep all tenure and rbind region data --------------
tenurebyre_region <- psrc_pums_count(pums_new_vars, group_vars = c("income_bin","PRACE","tenure"))
tenurebyre <- psrc_pums_count(pums_new_vars, group_vars = c("COUNTY","income_bin","PRACE","tenure")) %>%
  filter(COUNTY != "Region") %>% rbind(tenurebyre_region)

#-------------- Pivot the table --------------
tenurebyre_pivot <- tenurebyre %>% 
  pivot_wider(id_cols = c( 'DATA_YEAR', 'COUNTY', 'income_bin', 'PRACE'),
              names_from = 'tenure',
              values_from = c('count', 'count_moe', 'share', 'share_moe'))

#-------------- Write to Excel --------------

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "tenure by RE & income")
writeData(work_book, "tenure by RE & income", tenurebyre_pivot)
saveWorkbook(work_book, file = "Tenure by RE and Income/r_output.xlsx", overwrite = TRUE)