# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS 5YR
# DATE MODIFIED: 5.22.2023
# AUTHOR: Eric Clute

# Reviewer: Hana Sevcikova, 5/31/2023
# Review Notes:
#   Congratulations - it's a very nice piece of code! Below are some 
#   comments, suggestions and changes I made.
#   - Using setwd() within functions is a bad practice
#       - the working path should stay the same throughout the processing
#         (psrccensus has been now updated, so that changing path is not necessary)
#       - other users can have a file system where the paths are different
#       - for input/output operations, use file.path() instead
#   - Have a settings section at the beginning, so that users have only one code block to modify.
#   - Suggestion: if writing to files is not necessary every time, make it optional
#     (added the export_tables flag)
#   - Put all library calls at the beginning, so that there is no surprise 
#     of a missing library during processing.
#   - Put all functions on the top, followed by the main processing code
#     (alternatively, put functions into a separate file so that unit tests can be performed)
#   - Minimize running time-consuming operations. The original code was downloading the same data 
#     multiple times. I put the process of pulling data into a separate function that is run only once.
#     The data chunks are passed to the other functions as an argument.
#   - Avoid duplicating code.
#       - I put the code that creates the rent_burden column into a separate function, 
#         called create_rent_burden_column(), which is now called from both, rcb_re_func and rcb_inc_func.
#         That way, if you change your groupings, there is just one place to change it.
#       - Similarly for the cleaning code, it's now in a function, called summarize_and_clean().
#   - Avoid hard-coding as much as possible (I replaced the hard-coded column indices 
#     in summarize_and_clean by a string search).
#   - Good practice suggestion: put spaces between all objects, e.g. rr=TRUE -> rr = TRUE

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(tidyr)
library(purrr)
library(fredr)
library(psrcplot)
library(ggplot2)
library(openxlsx)

# SETTINGS
###########

# working directory
setwd("J:/Projects/V2050/Housing/Monitoring/2024Update/Data")
#setwd("~/psrc/housing-metrics/process/PUMS")

# years to include
years <- c(2010, 2016, 2022)
inflation_year <- 2022

# access FRED inflation data
fredr_set_key("99e2d81f189630d83b9e37ba8ca4f142")

# should the data be exported into an Excel file
export_tables <- TRUE 

# directory for the Excel file (only used if export_tables is TRUE)
output_dir <- file.path(getwd(), "metric19_cost_burden_by_race_inc") 
#output_dir <- getwd()

# Excel file name (only used if export_tables is TRUE)
output_table_name <- file.path(output_dir, "metric19_raw.xlsx")

########### end of settings
  
##### FUNCTIONS

# ---- Pull PUMS data
pull_data <- function(years){
  # returns a list of get_psrc_pums outputs, one for each year
  lapply(years, function(x) get_psrc_pums(5, x, "h",c("PRACE","TEN","GRPIP","HINCP")))
}

# create income/rent burden groupings
create_rent_burden_column <- function(GRPIP){
  factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                   between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                   GRPIP > 50 ~ "Greater than 50 percent",
                   !is.na(GRPIP) ~ "No rent paid"),
         levels=c("Greater than 50 percent",
                  "Between 30 and 50 percent",
                  "Less than 30 percent",
                  "No rent paid"))
}

# ----------------------------- SUMMARIZE BY RACE/ETHNICITY -----------------------------

rcb_re_func <- function(rcb){
  # Filter to only renters, create income/rent burden groupings, rename race/ethnicity categories, combine Some other Race & Two or More Races
  rcb <- rcb %>% filter(TEN=="Rented") %>%
    mutate(
      rent_burden = create_rent_burden_column(GRPIP),
      PRACE=factor(case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                             grepl("^Black ", PRACE) ~"Black",
                             grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                             !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                               stringr::str_replace(" alone", "") %>%
                               stringr::str_replace(" Alone", ""))))
  
  summarize_and_clean(rcb, c("PRACE","rent_burden"))
}

# ----------------------------- SUMMARIZE BY INCOME CATEGORY -----------------------------
rcb_inc_func <- function(rcb){
  
  # Adjust for inflation
  rcb <- real_dollars(rcb, inflation_year)
  
  # Filter to only renters, create income/rent burden groupings, rename income categories
  rcb <- rcb %>% filter(TEN=="Rented") %>%
    mutate(
      rent_burden = create_rent_burden_column(GRPIP),
      income_bin=factor(case_when(HINCP2022 < 25000 ~ "Under $25,000",
                                  HINCP2022 < 35000 ~ "$25,000-$34,999",
                                  HINCP2022 < 50000 ~ "$35,000-$49,999",
                                  HINCP2022 < 75000 ~ "$50,000-$74,999",
                                  HINCP2022 < 100000 ~ "$75,000-$99,999",
                                  HINCP2022 >=100000 ~ "$100,000 or more",
                                  !is.na(HINCP2022) ~ "Else / Prefer not to answer"),
                        levels=c("Under $25,000",
                                 "$25,000-$34,999",                                     
                                 "$35,000-$49,999",
                                 "$50,000-$74,999",
                                 "$75,000-$99,999",
                                 "$100,000 or more",
                                 "Else / Prefer not to answer"))
    )
  summarize_and_clean(rcb, c("income_bin","rent_burden"))
}

summarize_and_clean <- function(rcb, group_vars){
  # Summarize
  rcb_sum <- psrc_pums_count(rcb, group_vars = group_vars, rr=TRUE)
  rcb_sum <- rcb_sum %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe, reliability))

  # Clean table
  # remove all columns that start with "share_moe" and the COUNTY column
  rcb_cln <- rcb_sum[, -which(startsWith(colnames(rcb_sum), "share_moe") | colnames(rcb_sum) == "COUNTY")]
  # remove the row "Total"
  rcb_cln <- rcb_cln[rcb_cln[[group_vars[1]]] !='Total',]
  
  # Clean variable names
  rcb_cln <- rcb_cln %>% rename_all(~stringr::str_replace_all(.,"count_",""))
  rcb_cln <- rcb_cln %>% rename("No rent paid" = "NA")
  rcb_cln <- rcb_cln %>% rename("moe_No rent paid" = "moe_NA")
  rcb_cln <- rcb_cln %>% rename("Share_No rent paid" = "share_NA")
  rcb_cln <- rcb_cln %>% rename("Reliability_No rent paid" = "reliability_NA")
  rcb_cln
}
## End of functions

# PROCESSING
# -------------

# pull the data
rcb <- pull_data(years)

# Summarize by race/ethnicity
rcb_re_all <- map(rcb, ~rcb_re_func(.x)) %>%
  reduce(bind_rows)

# graph change over time -----------
rcb_re_severe_cb <- interactive_line_chart(rcb_re_all, "DATA_YEAR", "share_Greater than 50 percent", fill = "PRACE",
                                  title="Change in Severe Cost Burden by Race/Ethnicity (50%+ of income)",color="pgnobgy_10")
print(rcb_re_severe_cb)

rcb_re_cb <- interactive_line_chart(rcb_re_all, "DATA_YEAR", "share_Between 30 and 50 percent", fill = "PRACE",
                                  title="Change in Cost Burden by Race/Ethnicity (30-50% of income)",color="pgnobgy_10")
print(rcb_re_cb)


# Summarize by income
rcb_inc_all <- map(rcb, ~rcb_inc_func(.x)) %>%
  reduce(bind_rows)

# graph change over time -----------

# Removes NA rows - ensures the chart works correctly
rcb_inc_all_chart <- rcb_inc_all[!is.na(rcb_inc_all$`Greater than 50 percent`),]
rcb_inc_severe_cb <- static_line_chart(t=rcb_inc_all_chart,x="DATA_YEAR",y="share_Greater than 50 percent", fill="income_bin",
                                          title="Change in Severe Cost Burden by Income (50%+ of income)",color="pgnobgy_10")
print(rcb_inc_severe_cb)

rcb_inc_cb <- static_line_chart(rcb_inc_all_chart, "DATA_YEAR", "share_Between 30 and 50 percent", fill = "income_bin",
                                   title="Change in Cost Burden by Income (30-50% of income)",color="pgnobgy_10")
print(rcb_inc_cb)

# Exporting tables------------

if(export_tables){
  work_book <- createWorkbook()
  addWorksheet(work_book, sheetName = "rcb_re_all")
  writeData(work_book, "rcb_re_all", rcb_re_all)
  addWorksheet(work_book, sheetName = "rcb_inc_all")
  writeData(work_book, "rcb_inc_all", rcb_inc_all)
  saveWorkbook(work_book, file = output_table_name, overwrite = TRUE)
}
