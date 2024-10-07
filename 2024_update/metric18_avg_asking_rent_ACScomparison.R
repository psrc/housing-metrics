# Median Rent over Time
# Geographies: PSRC Counties
# Data: ACS 1 YR (2015-2023)
# Created by: Eric Clute

# ASSUMPTIONS
library(psrccensus)
library(magrittr)
library(purrr)
library(dplyr)


year <- c(2015,2016,2017,2018,2019,2021,2022,2023)

# FUNCTIONS
# ---- Pull PUMS data
pull_data <- function(years){
  # returns a list of get_psrc_pums outputs, one for each year
  lapply(years, function(x) get_acs_recs(geography = 'county',
                                         table.names = c('DP04'),
                                         years = x,
                                         counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                                         acs.type = 'acs1'))
}

combine_reduce <- function(rent_raw){
  rent_raw <- rent_raw %>% filter(`variable` == 'DP04_0134')
  
  }

# PROCESSING
# pull the data
rent_raw <- pull_data(year)

# Grab gross median rent and combine all years
rent_all <- map(rent_raw, ~combine_reduce(.x)) %>%
  reduce(bind_rows)

# CLEAN UP
# Reduce unneeded columns
rent_all <- rent_all %>% select(name, estimate, moe, label, acs_type, year, se, cv, reliability)
