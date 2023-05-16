# TITLE: Units in Structure by Tenure
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS Data 2006-2010 and 2017-2021
# LAST EDITED: 5.15.2023
# AUTHOR: Eric Clute & Christy Lam

library(psrccensus)
library(openxlsx)
library(tidycensus)
library(tidyverse)

# years of interest (applies to all functions below)
years <- c(2010,2021)

#---------------------OWNER OCCUPIED UNITS----------------------

create_uis_owner_summary_table <- function(year) {
  
  #---------------------Grab data from Census API------------------------
  
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  #---------------------Create custom groupings------------------------
  
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_003$", variable) ~ "Single Family", 
                                          grepl("_004$|_005$|_006$", variable) ~ "2-4 units",
                                          grepl("_007$|_008$", variable) ~ "5-19 units",
                                          grepl("_009$|_010$", variable) ~ "20+ units",
                                          grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-4 units","5-19 units", "20+ units", "Mobile Home/Other"))) %>% 
    mutate(building_size_2=factor(case_when(grepl("_003$", variable) ~ "Single Family",
                                            grepl("_004$|_005$|_006$|_007$|_008$", variable) ~ "2-19 units",
                                            grepl("_009$|_010$", variable) ~ "20+ units",
                                            grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                            TRUE ~ NA_character_),
                                  levels=c("Single Family","2-19 units", "20+ units", "Mobile Home/Other"))) %>%
    mutate(building_size_3=factor(case_when(grepl("_003$", variable) ~ "Single Family",
                                            grepl("_004$|_005$|_006$|_007$", variable) ~ "2-9 units",
                                            grepl("_008$", variable) ~ "10-19 units",
                                            grepl("_009$|_010$", variable) ~ "20+ units",
                                            grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                            TRUE ~ NA_character_),
                                  levels=c("Single Family","2-9 units", "10-19 units","20+ units" , "Mobile Home/Other")))
  
  #--------------------Aggregate data, incorporate 2-19 Unit group, 2-9 Unit group, and 10-19 unit group ------------------------
  
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg_owner <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_owner_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
  # In this step, you create an aggregate, using the 2-19 grouping
  uis_agg_owner_02 <- uis_coded %>%
    group_by(across(c(name, year, building_size_2))) %>%
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) %>% 
    filter(building_size_2 == '2-19 units') %>% 
    rename(building_size = building_size_2)
  
  # In this step, you create an aggregate, using the 2-9 grouping
  uis_agg_owner_03 <- uis_coded %>%
    group_by(across(c(name, year, building_size_3))) %>%
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) %>% 
    filter(building_size_3 == '2-9 units'| building_size_3 == '10-19 units') %>% 
    rename(building_size = building_size_3)
  
  df <- uis_agg_owner_01 %>% 
    bind_rows(list(uis_agg_owner_02, uis_agg_owner_03))
  
  return(df)
  
}

#---------------------Summarize data into one table, sort by building size------------------------

# iterate thru each year in the function, stored a list. Combine lists and order output by building size category

all_owner_tables <- map(years, ~create_uis_owner_summary_table(.x)) %>%
  reduce(bind_rows)

uis_owner <- all_owner_tables %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-4 units', '2-9 units', '5-19 units', '2-19 units', "10-19 units", '20+ units', 'Mobile Home/Other'))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

rm(list = setdiff(ls(), c("uis_owner", "create_uis_owner_summary_table", "years")))


#---------------------RENTER OCCUPIED UNITS------------------------

create_uis_renter_summary_table <- function(year) {
  
  #---------------------Grab data from Census API------------------------
  
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  #---------------------Create custom groupings------------------------
  
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_014$", variable) ~ "Single Family", 
                                          grepl("_015$|_016$|_017$", variable) ~ "2-4 units",
                                          grepl("_018$|_019$", variable) ~ "5-19 units",
                                          grepl("_020$|_021$", variable) ~ "20+ units",
                                          grepl("_022$|_023$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-4 units","5-19 units", "20+ units", "Mobile Home/Other"))) %>% 
    mutate(building_size_2=factor(case_when(grepl("_014$", variable) ~ "Single Family",
                                            grepl("_015$|_016$|_017$|_018$|_019$", variable) ~ "2-19 units",
                                            grepl("_020$|_021$", variable) ~ "20+ units",
                                            grepl("_022$|_023$", variable) ~ "Mobile Home/Other",
                                            TRUE ~ NA_character_),
                                  levels=c("Single Family","2-19 units", "20+ units", "Mobile Home/Other"))) %>%
    mutate(building_size_3=factor(case_when(grepl("_014$", variable) ~ "Single Family",
                                            grepl("_015$|_016$|_017$|_018$", variable) ~ "2-9 units",
                                            grepl("_019$", variable) ~ "10-19 units",
                                            grepl("_020$|_021$", variable) ~ "20+ units",
                                            grepl("_022$|_023$", variable) ~ "Mobile Home/Other",
                                            TRUE ~ NA_character_),
                                  levels=c("Single Family","2-9 units","10-19 units", "20+ units", "Mobile Home/Other")))

  #--------------------Aggregate data, incorporate 2-19 Unit group------------------------
  
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg_renter <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_renter_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
  # In this step, you create an aggregate, using the 2-9 grouping
  uis_agg_renter_02 <- uis_coded %>%
    group_by(across(c(name, year, building_size_2))) %>%
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) %>% 
    filter(building_size_2 == '2-19 units') %>% 
    rename(building_size = building_size_2)
  
  # In this step, you create an aggregate, using the 2-9 grouping
  uis_agg_renter_03 <- uis_coded %>%
    group_by(across(c(name, year, building_size_3))) %>%
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) %>% 
    filter(building_size_3 == '2-9 units'| building_size_3 == '10-19 units') %>% 
    rename(building_size = building_size_3)
  
  df <- uis_agg_renter_01 %>% 
    bind_rows(list(uis_agg_renter_02,uis_agg_renter_03))
  
}

#---------------------Summarize data into one table, sort by building size------------------------

# iterate thru each year in the function, stored a list. Combine lists and order output by building size category

all_renter_tables <- map(years, ~create_uis_renter_summary_table(.x)) %>% 
  reduce(bind_rows)

uis_renter <- all_renter_tables %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-4 units', '2-9 units', '5-19 units', '2-19 units', "10-19 units", '20+ units', 'Mobile Home/Other'))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

rm(list = setdiff(ls(), c("uis_renter", "create_uis_renter_summary_table", "uis_owner", "create_uis_owner_summary_table", "years")))

#------------------------Summarize existing housing stock------------------------


calc_share_growth <- function(table) {
  
  # calculate totals in new dataframe
  totals <- table %>% 
    filter(!(building_size %in% c('2-9 units', '2-19 units','10-19 units'))) %>%
    group_by(name, year) %>% 
    summarise(total = sum(estimate))
  
  # join to main table
  table <- table %>% 
    left_join(totals, by = c('name', 'year'))
  
  # calculate % of units by building size and % growth between years
  table <- table %>% 
    mutate(share = estimate/total) %>% 
    arrange(name, building_size) %>% 
    group_by(name, building_size) %>% 
    mutate(growth = estimate-lag(estimate)) %>% 
    mutate(total_diff = total - lag(total)) %>% 
    mutate(growth_share = growth/total_diff) %>% 
    arrange(factor(name, levels = c('King County', 'Kitsap County', 'Pierce County', 'Snohomish County', 'Region')))
}

pivot_to_wide <- function(table) {
  
  # wide format
  growth_cols_head <- c(paste0('growth_', years[1:(length(years)-1)]), paste0('growth_share_', years[1:(length(years)-1)]))
  growth_cols_tail <- rep(str_sub(years[-1], start = 3, end = 4), 2)
  
  new_colnames <- map2(growth_cols_head, growth_cols_tail, ~paste0(.x, '-', .y)) %>% unlist()
  old_colnames <- c(paste0('growth_', years[-1]), paste0('growth_share_', years[-1]))
  names(old_colnames) <- new_colnames
  
  # pivot to wide format
  df <- table %>% 
    pivot_wider(id_cols = c('name', 'building_size'),
                names_from = year,
                values_from = c('estimate', 'moe', 'total', 'share', 'growth', 'growth_share')) 
  
  # rename growth columns
  df <- df %>% 
    rename(all_of(old_colnames)) %>% 
    select(-ends_with(paste0('growth_', years[1])), -ends_with(paste0('growth_share_', years[1])))
}

# long format
df_uis_owner <- calc_share_growth(uis_owner)
df_uis_renter <- calc_share_growth(uis_renter)

# wide format (for excel)
df_uis_owner_wide <- pivot_to_wide(df_uis_owner)
df_uis_renter_wide <- pivot_to_wide(df_uis_renter)

share_cols_owner <- str_which(colnames(df_uis_owner_wide), 'share')
share_cols_renter <- str_which(colnames(df_uis_renter_wide), 'share')

# Add RR scores
df_uis_owner_wide$rr_score_2010 <- (df_uis_owner_wide$moe_2010/1.645)/df_uis_owner_wide$estimate_2010*100
df_uis_owner_wide$rr_score_2021 <- (df_uis_owner_wide$moe_2021/1.645)/df_uis_owner_wide$estimate_2021*100
df_uis_owner_wide <- df_uis_owner_wide %>%
  mutate(rr_score_2010=factor(case_when(rr_score_2010 <= 15 ~"good",
                                        rr_score_2010 <= 30 ~"fair",
                                        rr_score_2010 <= 50 ~"weak",
                                        rr_score_2010 > 50 ~"unreliable",
                                       !is.na(rr_score_2010) ~ NA)),
         rr_score_2021=factor(case_when(rr_score_2021 <= 15 ~"good",
                                        rr_score_2021 <= 30 ~"fair",
                                        rr_score_2021 <= 50 ~"weak",
                                        rr_score_2021 > 50 ~"unreliable",
                                        !is.na(rr_score_2021) ~ NA)))

df_uis_renter_wide$rr_score_2010 <- (df_uis_renter_wide$moe_2010/1.645)/df_uis_renter_wide$estimate_2010*100
df_uis_renter_wide$rr_score_2021 <- (df_uis_renter_wide$moe_2021/1.645)/df_uis_renter_wide$estimate_2021*100
df_uis_renter_wide <- df_uis_renter_wide %>%
  mutate(rr_score_2010=factor(case_when(rr_score_2010 <= 15 ~"good",
                                        rr_score_2010 <= 30 ~"fair",
                                        rr_score_2010 <= 50 ~"weak",
                                        rr_score_2010 > 50 ~"unreliable",
                                        !is.na(rr_score_2010) ~ NA)),
         rr_score_2021=factor(case_when(rr_score_2021 <= 15 ~"good",
                                        rr_score_2021 <= 30 ~"fair",
                                        rr_score_2021 <= 50 ~"weak",
                                        rr_score_2021 > 50 ~"unreliable",
                                        !is.na(rr_score_2021) ~ NA)))

#------------------------Export tables for Excel------------------------
work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "Owners 5YR ACS")
addWorksheet(work_book, sheetName = "Renters 5YR ACS")
writeData(work_book, "Owners 5YR ACS", df_uis_owner_wide)
writeData(work_book, "Renters 5YR ACS", df_uis_renter_wide)

# Create a percent style
pct = createStyle(numFmt="PERCENTAGE")

# Add the percent style to the desired cells
addStyle(work_book, "Owners 5YR ACS", style=pct, cols=share_cols_owner, rows=2:(nrow(df_uis_owner_wide)+1), gridExpand=TRUE)
addStyle(work_book, "Renters 5YR ACS", style=pct, cols=share_cols_renter, rows=2:(nrow(df_uis_renter_wide)+1), gridExpand=TRUE)

setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")
saveWorkbook(work_book, file = "Units In Structure by Tenure/r_output.xlsx", overwrite = TRUE)

#------------------------Comparisons Over Time (moderate density)------------------------
compared_years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)

create_uis_renter_compare <- function(compared_year) {
  
  #---------------------Grab data from Census API
  
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = compared_year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  #---------------------Create custom groupings
  
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_014$", variable) ~ "Single Family",
                                            grepl("_015$|_016$|_017$|_018$", variable) ~ "2-9 units",
                                            grepl("_019$", variable) ~ "10-19 units",
                                            grepl("_020$|_021$", variable) ~ "20+ units",
                                            grepl("_022$|_023$", variable) ~ "Mobile Home/Other",
                                            TRUE ~ NA_character_),
                                  levels=c("Single Family","2-9 units","10-19 units", "20+ units", "Mobile Home/Other")))
  
  #--------------------Aggregate data
  
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg_renter <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_renter_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
}

#---------------------Summarize data into one table, sort by building size

# iterate thru each year in the function, stored a list. Combine lists and order output by building size category

all_renter_compare <- map(compared_years, ~create_uis_renter_compare(.x)) %>% 
  reduce(bind_rows)

all_renter_compare$rr_score <- (all_renter_compare$moe/1.645)/all_renter_compare$estimate*100

all_renter_compare <- all_renter_compare %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-9 units', "10-19 units", '20+ units', 'Mobile Home/Other')),
         rr_score=factor(case_when(rr_score <= 15 ~"good",
                               rr_score <= 30 ~"fair",
                               rr_score <= 50 ~"weak",
                               rr_score > 50 ~"unreliable",
                               !is.na(rr_score) ~ NA))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

#---------------------Chart by density of units, per county, over time (should not compare overlapping 5 YR estimates!!)
library(psrcplot)
library(ggplot2)

middle_density <- all_renter_compare %>%
  filter(building_size == '2-9 units') %>%
  ungroup()

mid_density_chart <- interactive_line_chart(middle_density, "year", "estimate", fill = "name",
                                            title="Middle Density Renter Units (2-9)",color="pgnobgy_10")
mid_density_chart

#---------------------Chart out moderate density units, per county, over time
moderate_density <- all_renter_compare %>%
  filter(building_size == '10-19 units') %>%
  ungroup()

mod_density_chart <- interactive_line_chart(moderate_density, "year", "estimate", fill = "name",
                                title="Moderate Density Renter Units (10-19)",color="pgnobgy_10")
mod_density_chart

#---------------------Chart out high density units, per county, over time
high_density <- all_renter_compare %>%
  filter(building_size == '20+ units') %>%
  ungroup()

high_density_chart <- interactive_line_chart(high_density, "year", "estimate", fill = "name",
                                            title="High Density Renter Units (20+)",color="pgnobgy_10")
high_density_chart

#---------------------Chart out single family units, per county, over time
single_family <- all_renter_compare %>%
  filter(building_size == 'Single Family') %>%
  ungroup()

single_family_chart <- interactive_line_chart(single_family, "year", "estimate", fill = "name",
                                             title="Single Family Renter Units",color="pgnobgy_10")
single_family_chart

#---------------------Chart out mobile home/other units, per county, over time
mh_other <- all_renter_compare %>%
  filter(building_size == 'Mobile Home/Other') %>%
  ungroup()

mh_other_chart <- interactive_line_chart(mh_other, "year", "estimate", fill = "name",
                                              title="Mobile Home/Other Renter Units",color="pgnobgy_10")
mh_other_chart