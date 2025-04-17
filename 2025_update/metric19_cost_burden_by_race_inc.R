# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS 5YR
# DATE MODIFIED: 4.17.2025
# AUTHOR: Eric Clute

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
save_path<- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric19_cost_burden_by_race_inc/"

# years to include
years <- c(2010, 2016, 2023)
inflation_year <- 2023
final_year_for_re_smalltbl <- 2023
years_for_inc_smalltbl <- c(2010, 2023)

# access FRED inflation data
fredr_set_key("99e2d81f189630d83b9e37ba8ca4f142")

# should the data be exported into an Excel file
export_tables <- TRUE 

# directory for the Excel file (only used if export_tables is TRUE)
output_dir <- file.path(save_path) 

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

# SUMMARIZE BY RACE/ETHNICITY -----------------------------

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

# SUMMARIZE BY INCOME CATEGORY -----------------------------
rcb_inc_func <- function(rcb){
  
  # Adjust for inflation
  rcb <- real_dollars(rcb, inflation_year)
  
  # Filter to only renters, create income/rent burden groupings, rename income categories
  rcb <- rcb %>% filter(TEN=="Rented") %>%
    mutate(
      rent_burden = create_rent_burden_column(GRPIP),
      income_bin=factor(case_when(HINCP2023 < 25000 ~ "Under $25,000",
                                  HINCP2023 < 35000 ~ "$25,000-$34,999",
                                  HINCP2023 < 50000 ~ "$35,000-$49,999",
                                  HINCP2023 < 75000 ~ "$50,000-$74,999",
                                  HINCP2023 < 100000 ~ "$75,000-$99,999",
                                  HINCP2023 >=100000 ~ "$100,000 or more",
                                  !is.na(HINCP2023) ~ "Else / Prefer not to answer"),
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

# create small tables for export -----------
rcb_re_smalltbl <- rcb_re_all %>%
  filter(DATA_YEAR == final_year_for_re_smalltbl) %>%
  select(DATA_YEAR, PRACE, `share_Greater than 50 percent`, `share_Between 30 and 50 percent`)

rcb_inc_smalltbl <- rcb_inc_all %>%
  filter(DATA_YEAR %in% years_for_inc_smalltbl) %>%
  pivot_wider(id_cols = income_bin, names_from = DATA_YEAR , values_from = c(`share_Between 30 and 50 percent`,`share_Greater than 50 percent`))

# Exporting tables------------

if(export_tables){
  work_book <- createWorkbook()
  addWorksheet(work_book, sheetName = "rcb_re")
  writeData(work_book, "rcb_re", rcb_re_smalltbl)
  addWorksheet(work_book, sheetName = "rcb_inc")
  writeData(work_book, "rcb_inc", rcb_inc_smalltbl)
  saveWorkbook(work_book, file = output_table_name, overwrite = TRUE)
}
