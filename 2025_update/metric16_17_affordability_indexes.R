# TITLE: Housing Affordability Indexes - Median Buyer & First Time Buyer
# GEOGRAPHIES: King, Kitsap, Snohomish, Pierce Counties
# DATA SOURCE: WCRER (Washington Center for Real Estate Research)
# DATE MODIFIED: 5.15.2025
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

# "https://public.tableau.com/app/profile/mason.virant/viz/County_DB_HAIMedianBuyer_Q1_2025/DB-HAIMedianBuyer?County=King,Kitsap,Pierce,Snohomish"
# "https://public.tableau.com/app/profile/mason.virant/viz/County_DB_HAIFirstTimeBuyer_Q1_2025/DB-HAIFirstTimeBuyer?County=King,Kitsap,Pierce,Snohomish"

mb_hai_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric16_17_affordability_indexes/raw/hai_median_data.csv"
ftb_hai_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric16_17_affordability_indexes/raw/hai_firsttime_data.csv"
output_file <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric16_17_affordability_indexes/r_export_hai.xlsx"

quarter <- "Q1"  #which quarter are we interested in?

# --------------- Create function(s) ---------------
filter_and_pivot_by_quarter <- function(df, quarter) {
  # Identify relevant column names
  period_col <- names(df)[startsWith(names(df), "Period")]
  county_col <- names(df)[startsWith(names(df), "County")]
  hai_col    <- names(df)[startsWith(names(df), "HAI")]
  
  # Filter and pivot
  df %>%
    filter(str_ends(.data[[period_col]], quarter)) %>%
    select(all_of(c(period_col, county_col, hai_col))) %>%
    pivot_wider(
      names_from = all_of(county_col),
      values_from = all_of(hai_col)
    )
}

# --------------- Pull Data ---------------
mb_hai <- read.csv(mb_hai_path)
ftb_hai <- read.csv(ftb_hai_path)

# --------------- Run Function, clean data ---------------
mb_hai_filtered <- filter_and_pivot_by_quarter(mb_hai,quarter)
ftb_hai_filtered <- filter_and_pivot_by_quarter(ftb_hai,quarter)

# --------------- Export to Excel ---------------
write_xlsx(
  list(
    "mb_hai_filtered" = mb_hai_filtered,
    "ftb_hai_filtered" = ftb_hai_filtered
  ),
  path = output_file
)
