# TITLE: Ownership Opportunities Near HCTs & Centers
# GEOGRAPHIES: PSRC Region - Block Group
# DATA SOURCE: 5YR ACS Data 2017-21
# LAST EDITED: 3.29.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)
library(openxlsx)

setwd("C:/Users/eclute/Downloads")

#------------ Collect HH Tenure by Block Group ------------
B25003_raw <- get_acs_recs(geography = 'block group',
                           table.names = c('B25003'),
                           years = 2021,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

tenure_bg <- B25003_raw %>%
  filter(variable == "B25003_001" | variable == "B25003_002" | variable == "B25003_003") %>%
  mutate(.keep = "none",
         geoid = GEOID,
         geography = census_geography,
         estimate = estimate,
         moe = moe,
         variable = factor(case_when(variable == "B25003_001" ~ "total households",
                                     variable == "B25003_002" ~ "owner",
                                     variable == "B25003_003" ~ "renter",
                                     !is.na(variable) ~"N/A"),
                           levels=c("total households","owner","renter")))

tenure_bg <- tenure_bg %>% 
  pivot_wider(id_cols = c( 'geography', 'geoid'),
              names_from = 'variable',
              values_from = c('estimate', 'moe'))

#------------ Collect bedroom count by Tenure by Block Group ------------

B25042_raw <- get_acs_recs(geography = 'block group',
                           table.names = c('B25042'),
                           years = 2021,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

bedroomcount_bg <- B25042_raw %>%
  mutate(.keep = "none",
         geoid = GEOID,
         name = name,
         variable = variable,
         estimate = estimate,
         moe = moe,
         label = label,
         concept = concept,
         acs_type = acs_type,
         year = year) 

bedroomcount_bg <- bedroomcount_bg %>% 
  mutate(label_copy := label,
         label_content = str_replace_all(label_copy, 'Estimate!!Total:(!!)*', "")) %>% 
  mutate(tenure = ifelse(label_content == "", 'All Tenure', str_extract(label_content, '^(\\w+)')),
         bedrooms = str_extract(label_content, '(?<=!!)(\\d)*(\\s+)*(\\w+)+(\\s\\w+)*')) %>% 
  mutate(bedrooms = ifelse(tenure != 'All Tenure' & is.na(bedrooms), paste('Total Bedrooms'), bedrooms),
         bedrooms = ifelse(tenure == 'All Tenure' & is.na(bedrooms), 'Total Bedrooms', bedrooms)) %>% 
  select(-label_copy, -label_content) 

bedroomcount_bg <- bedroomcount_bg %>% 
  pivot_wider(id_cols = c('acs_type', 'year', 'geoid', 'name'),
              names_from = c('tenure', 'bedrooms'),
              values_from = c('estimate', 'moe'))


#------------ Collect reported home value by Block Group ------------

B25075_raw <- get_acs_recs(geography = 'block group',
                           table.names = c('B25075'),
                           years = 2021,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

homevalue_bg <- B25075_raw %>%
  mutate(.keep = "none",
         geoid = GEOID,
         name = name,
         variable = variable,
         estimate = estimate,
         moe = moe,
         label = label,
         concept = concept,
         acs_type = acs_type,
         year = year)

homevalue_bg <- homevalue_bg %>% 
  mutate(homevalue = str_replace_all(label, 'Estimate!!Total:(!!)*', "")) %>% 
  mutate(homevalue = ifelse(homevalue == "", 'Total', homevalue))

homevalue_bg <- homevalue_bg %>% 
  pivot_wider(id_cols = c('acs_type', 'year', 'geoid', 'name'),
              names_from = c('homevalue'),
              values_from = c('estimate', 'moe'))

#-------------- Write to Excel --------------
setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "bedroomcount_bg")
addWorksheet(work_book, sheetName = "tenure_bg")
addWorksheet(work_book, sheetName = "homevalue_bg")
writeData(work_book, "bedroomcount_bg", bedroomcount_bg)
writeData(work_book, "tenure_bg", tenure_bg)
writeData(work_book, "homevalue_bg", homevalue_bg)
saveWorkbook(work_book, file = "Ownership Opportunities Near Centers HCTs/r_output 20215YR.xlsx", overwrite = TRUE)