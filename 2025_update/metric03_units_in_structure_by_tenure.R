# TITLE: Units in Structure by Tenure
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS Data
# LAST EDITED: 4.09.2025
# AUTHOR: Eric Clute & Christy Lam

library(psrccensus)
library(openxlsx)
library(tidycensus)
library(tidyverse)
library(psrcplot)
library(ggplot2)

# Assumptions
setwd("J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric03_units_in_structure_by_tenure")
years <- c(2010,2023) # base year and final year
compared_years <- c(2010,2016,2023) # comparison years over time

# FUNCTIONS

# Formatting, calculating change in share
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

# Owner Occupied
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
create_uis_owner_compare <- function(compared_year) {
  
  # Grab data from Census API
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = compared_year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  # Create custom groupings
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_003$", variable) ~ "Single Family",
                                          grepl("_004$|_005$|_006$|_007$", variable) ~ "2-9 units",
                                          grepl("_008$", variable) ~ "10-19 units",
                                          grepl("_009$|_010$", variable) ~ "20+ units",
                                          grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-9 units","10-19 units", "20+ units", "Mobile Home/Other")))
  
  # Aggregate data
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg_owner <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_owner_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
}

# Renter Occupied
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
create_uis_renter_compare <- function(compared_year) {
  
  # Grab data from Census API
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = compared_year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  # Create custom groupings
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_014$", variable) ~ "Single Family",
                                          grepl("_015$|_016$|_017$|_018$", variable) ~ "2-9 units",
                                          grepl("_019$", variable) ~ "10-19 units",
                                          grepl("_020$|_021$", variable) ~ "20+ units",
                                          grepl("_022$|_023$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-9 units","10-19 units", "20+ units", "Mobile Home/Other")))
  
  # Aggregate data
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg_renter <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_renter_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
}

# Compare Over Time
create_uis_compare <- function(compared_year) {
  
  # Grab data from Census API
  uis_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years = compared_year,
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs5')
  
  # Create custom groupings
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  
  uis_coded <- uis_raw %>% 
    mutate(building_size=factor(case_when(grepl("_003$|_014$", variable) ~ "Single Family",
                                          grepl("_004$|_005$|_006$|_007$|_015$|_016$|_017$|_018$", variable) ~ "2-9 units",
                                          grepl("_008$|_019$", variable) ~ "10-19 units",
                                          grepl("_009$|_010$|_020$|_021$", variable) ~ "20+ units",
                                          grepl("_011$|_012$|_022$|_023$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-9 units","10-19 units", "20+ units", "Mobile Home/Other")))
  
  # Aggregate data
  # In this step, you create an aggregate, using the grouping you created in the last call.
  uis_agg <- summarize(uis_raw, estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) 
  
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  uis_agg_01 <- uis_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
}

# ANALYSES 
# Owners UIS table ------------------------
all_owner_tables <- map(years, ~create_uis_owner_summary_table(.x)) %>%
  reduce(bind_rows)

uis_owner <- all_owner_tables %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-4 units', '2-9 units', '5-19 units', '2-19 units', "10-19 units", '20+ units', 'Mobile Home/Other'))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

# Renters UIS table ------------------------
all_renter_tables <- map(years, ~create_uis_renter_summary_table(.x)) %>% 
  reduce(bind_rows)

uis_renter <- all_renter_tables %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-4 units', '2-9 units', '5-19 units', '2-19 units', "10-19 units", '20+ units', 'Mobile Home/Other'))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

# Summarize existing housing stock------------------------
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
df_uis_owner_wide$rr_score_2023 <- (df_uis_owner_wide$moe_2023/1.645)/df_uis_owner_wide$estimate_2023*100
df_uis_owner_wide <- df_uis_owner_wide %>%
  mutate(rr_score_2010=factor(case_when(rr_score_2010 <= 15 ~"good",
                                        rr_score_2010 <= 30 ~"fair",
                                        rr_score_2010 <= 50 ~"weak",
                                        rr_score_2010 > 50 ~"unreliable",
                                       !is.na(rr_score_2010) ~ NA)),
         rr_score_2023=factor(case_when(rr_score_2023 <= 15 ~"good",
                                        rr_score_2023 <= 30 ~"fair",
                                        rr_score_2023 <= 50 ~"weak",
                                        rr_score_2023 > 50 ~"unreliable",
                                        !is.na(rr_score_2023) ~ NA)))

df_uis_renter_wide$rr_score_2010 <- (df_uis_renter_wide$moe_2010/1.645)/df_uis_renter_wide$estimate_2010*100
df_uis_renter_wide$rr_score_2023 <- (df_uis_renter_wide$moe_2023/1.645)/df_uis_renter_wide$estimate_2023*100
df_uis_renter_wide <- df_uis_renter_wide %>%
  mutate(rr_score_2010=factor(case_when(rr_score_2010 <= 15 ~"good",
                                        rr_score_2010 <= 30 ~"fair",
                                        rr_score_2010 <= 50 ~"weak",
                                        rr_score_2010 > 50 ~"unreliable",
                                        !is.na(rr_score_2010) ~ NA)),
         rr_score_2023=factor(case_when(rr_score_2023 <= 15 ~"good",
                                        rr_score_2023 <= 30 ~"fair",
                                        rr_score_2023 <= 50 ~"weak",
                                        rr_score_2023 > 50 ~"unreliable",
                                        !is.na(rr_score_2023) ~ NA)))

# Format small table to be exported to Infogram ------------------------

df_uis_owner_smalltbl <- pivot_wider(df_uis_owner_wide,id_cols = name, names_from = building_size, values_from = `growth_2010-23`)
df_uis_owner_smalltbl <- df_uis_owner_smalltbl %>% 
  select(name, `Single Family`, `2-19 units`, `20+ units`, `Mobile Home/Other`)

df_uis_renter_smalltbl <- pivot_wider(df_uis_renter_wide,id_cols = name, names_from = building_size, values_from = `growth_2010-23`)
df_uis_renter_smalltbl <- df_uis_renter_smalltbl %>% 
  select(name, `Single Family`, `2-19 units`, `20+ units`, `Mobile Home/Other`)

# CHARTS - COMPARISONS OVER TIME

# All Occupied Units------------------------
# Summarize data into one table, sort by building size
all_compare <- map(compared_years, ~create_uis_compare(.x)) %>% 
  reduce(bind_rows)

all_compare$rr_score <- (all_compare$moe/1.645)/all_compare$estimate*100

all_compare <- all_compare %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-9 units', "10-19 units", '20+ units', 'Mobile Home/Other')),
         rr_score=factor(case_when(rr_score <= 15 ~"good",
                               rr_score <= 30 ~"fair",
                               rr_score <= 50 ~"weak",
                               rr_score > 50 ~"unreliable",
                               !is.na(rr_score) ~ NA))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

# Chart by density of units, per county, over time (should not compare overlapping 5 YR estimates!!)
middle_density <- all_compare %>%
  filter(building_size == '2-9 units') %>%
  ungroup()

mid_density_chart <- interactive_line_chart(middle_density, "year", "estimate", fill = "name",
                                            title="Middle Density Units (2-9)",color="pgnobgy_10")
mid_density_chart

# Chart out moderate density units, per county, over time
moderate_density <- all_compare %>%
  filter(building_size == '10-19 units') %>%
  ungroup()

mod_density_chart <- interactive_line_chart(moderate_density, "year", "estimate", fill = "name",
                                title="Moderate Density Units (10-19)",color="pgnobgy_10")
mod_density_chart

# Renter Occupied Charts -----------------------
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

# Chart by density of units, per county, over time (should not compare overlapping 5 YR estimates!!)
middle_density <- all_renter_compare %>%
  filter(building_size == '2-9 units') %>%
  ungroup()

mid_density_chart <- interactive_line_chart(middle_density, "year", "estimate", fill = "name",
                                            title="Middle Density Renter Units (2-9)",color="pgnobgy_10")
mid_density_chart

# Chart out moderate density units, per county, over time
moderate_density <- all_renter_compare %>%
  filter(building_size == '10-19 units') %>%
  ungroup()

mod_density_chart <- interactive_line_chart(moderate_density, "year", "estimate", fill = "name",
                                            title="Moderate Density Renter Units (10-19)",color="pgnobgy_10")
mod_density_chart

# Owner Occupied Charts -----------------------
all_owner_compare <- map(compared_years, ~create_uis_owner_compare(.x)) %>% 
  reduce(bind_rows)

all_owner_compare$rr_score <- (all_owner_compare$moe/1.645)/all_owner_compare$estimate*100

all_owner_compare <- all_owner_compare %>% 
  mutate(building_size = factor(building_size,
                                levels = c('Single Family', '2-9 units', "10-19 units", '20+ units', 'Mobile Home/Other')),
         rr_score=factor(case_when(rr_score <= 15 ~"good",
                                   rr_score <= 30 ~"fair",
                                   rr_score <= 50 ~"weak",
                                   rr_score > 50 ~"unreliable",
                                   !is.na(rr_score) ~ NA))) %>% 
  arrange(year, name, building_size) %>% 
  filter(building_size != is.na(building_size))

# Chart by density of units, per county, over time (should not compare overlapping 5 YR estimates!!)
middle_density <- all_owner_compare %>%
  filter(building_size == '2-9 units') %>%
  ungroup()

mid_density_chart <- interactive_line_chart(middle_density, "year", "estimate", fill = "name",
                                            title="Middle Density Owner Units (2-9)",color="pgnobgy_10")
mid_density_chart

# Chart out moderate density units, per county, over time
moderate_density <- all_owner_compare %>%
  filter(building_size == '10-19 units') %>%
  ungroup()

mod_density_chart <- interactive_line_chart(moderate_density, "year", "estimate", fill = "name",
                                            title="Moderate Density Owner Units (10-19)",color="pgnobgy_10")
mod_density_chart

# Export tables for Excel------------------------
work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "Owners 5YR ACS")
addWorksheet(work_book, sheetName = "Renters 5YR ACS")
writeData(work_book, "Owners 5YR ACS", df_uis_owner_smalltbl)
writeData(work_book, "Renters 5YR ACS", df_uis_renter_smalltbl)

# Create a percent style
pct = createStyle(numFmt="PERCENTAGE")

# Add the percent style to the desired cells
addStyle(work_book, "Owners 5YR ACS", style=pct, cols=share_cols_owner, rows=2:(nrow(df_uis_owner_smalltbl)+1), gridExpand=TRUE)
addStyle(work_book, "Renters 5YR ACS", style=pct, cols=share_cols_renter, rows=2:(nrow(df_uis_renter_smalltbl)+1), gridExpand=TRUE)
saveWorkbook(work_book, file = "metric03_raw.xlsx", overwrite = TRUE)
