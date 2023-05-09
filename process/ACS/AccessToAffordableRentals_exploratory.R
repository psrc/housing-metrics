# TITLE: Affordable Rental Housing
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: 5YR ACS Data
# LAST EDITED: 5.8.2023
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)
library(tidyr)

years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
years <- c(2015,2021)
setwd("C:/Users/eclute/Downloads")

#------------ Median Gross Rent ------------
grossrent_func <- function(year){
  
  setwd("C:/Users/eclute/Downloads")
  pums_raw <- get_psrc_pums(5,year,"h",c("TEN","GRNTP","HINCP"))
  pums <- pums_raw
  
  # Analysis
  grossrent <- psrc_pums_median(pums, "GRNTP", group_vars = c("DATA_YEAR"), rr=TRUE)
  grossrent <- grossrent[grossrent$DATA_YEAR !='Total',]
  
}

grossrent <- map(years, ~grossrent_func(.x)) %>%
  reduce(bind_rows)

#------------ Median Renter HH Income ------------
inc_func <- function(year){
  
  setwd("C:/Users/eclute/Downloads")
  pums_raw <- get_psrc_pums(5,year,"h",c("TEN","HINCP"))
  pums <- pums_raw
  
  # Analysis
  inc <- psrc_pums_median(pums, "HINCP", group_vars = c("DATA_YEAR","TEN"),rr=TRUE)
  inc <- filter(inc, TEN == "Rented")
  inc$maxmonthlyrent <- inc$HINCP_median/12*0.3
  
}

inc <- map(years, ~inc_func(.x)) %>%
  reduce(bind_rows)
