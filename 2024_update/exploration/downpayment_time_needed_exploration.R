# TITLE: Time needed to save for downpayment on median home
# GEOGRAPHIES: King, Snohomish, Pierce (limited by Zillow)
# DATA SOURCE: FreddieMac, Zillow, ACS
# DATE MODIFIED: 4.19.2024
# AUTHOR: Eric Clute

# Assumptions

avg_mrtg_pmnt <- "C:/Users/eclute/GitHub/housing-metrics/2024_update/metric21_avg_mortgage_payment.R"
source(avg_mrtg_pmnt)
years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022)

# Function

inc_func <- function(year){
  # Pull in income data (ACS)
  inc_raw <- get_acs_recs(geography = 'msa', table.names = 'S1903', years = year, acs.type = 'acs1')
  
  inc <- inc_raw %>%
    filter(variable == "S1903_C03_001" & name == "Seattle-Tacoma-Bellevue")
}

# Analysis
inc_all <- map(years, ~inc_func(.x)) %>%
  reduce(bind_rows)



# Need to track down changes to variable name prior to 2017. 