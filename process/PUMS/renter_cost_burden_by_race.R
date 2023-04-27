# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS 5YR
# DATE MODIFIED: 4.26.2023
# AUTHOR: Eric Clute

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(tidyr)
library(purrr)

years <- c(2010, 2016, 2021)

# ----------------------------- SUMMARIZE BY RACE/ETHNICITY -----------------------------
rcb_re_func <- function(year){
  
  # Pull PUMS data
  setwd("C:/Users/eclute/Downloads")
  rcb_raw <- get_psrc_pums(5,year,"h",c("PRACE","TEN","GRPIP","HINCP"))
  setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")
  rcb <- rcb_raw
  
  # Filter to only renters, create income/rent burden groupings, rename race/ethnicity categories, combine Some other Race & Two or More Races
  rcb <- rcb %>% filter(TEN=="Rented") %>%
    mutate(
      rent_burden=factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                   between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                                   GRPIP > 50 ~ "Greater than 50 percent",
                                   !is.na(GRPIP) ~ "No rent paid"),
                         levels=c("Greater than 50 percent",
                                  "Between 30 and 50 percent",
                                  "Less than 30 percent",
                                  "No rent paid")),
      PRACE=factor(case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                             grepl("^Black ", PRACE) ~"Black",
                             grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                             !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                               stringr::str_replace(" alone", "") %>%
                               stringr::str_replace(" Alone", ""))))

  # Summarize
  rcb_re <- psrc_pums_count(rcb, group_vars = c("PRACE","rent_burden"),rr=TRUE)
  rcb_re <- rcb_re %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe, reliability))
  
  # Clean table
  rcb_re <- rcb_re[, c(1,3,4,5,6,8,7,9,10,11,13,12,14,15,16,18,17,24,25,26,28,27)]
  rcb_re <- rcb_re[rcb_re$PRACE !='Total',]
  
  # Clean variable names
  rcb_re <- rcb_re %>% rename_all(~stringr::str_replace_all(.,"count_",""))
  rcb_re <- rcb_re %>% rename("No rent paid" = "NA")
  rcb_re <- rcb_re %>% rename("moe_No rent paid" = "moe_NA")
  rcb_re <- rcb_re %>% rename("Share_No rent paid" = "share_NA")
  rcb_re <- rcb_re %>% rename("Reliability_No rent paid" = "reliability_NA")
  
}

# Run rcb function -----------
rcb_re_all <- map(years, ~rcb_re_func(.x)) %>%
  reduce(bind_rows)

# graph change over time -----------
library(psrcplot)
library(ggplot2)

rcb_re_severe_cb <- interactive_line_chart(rcb_re_all, "DATA_YEAR", "share_Greater than 50 percent", fill = "PRACE",
                                  title="Change in Severe Cost Burden by Race/Ethnicity",color="pgnobgy_5")
rcb_re_severe_cb

rcb_re_cb <- interactive_line_chart(rcb_re_all, "DATA_YEAR", "share_Between 30 and 50 percent", fill = "PRACE",
                                       title="Change in Cost Burden by Race/Ethnicity",color="pgnobgy_5")
rcb_re_cb

# ----------------------------- SUMMARIZE BY INCOME CATEGORY -----------------------------
rcb_inc_func <- function(year){
  
  # Pull PUMS data
  setwd("C:/Users/eclute/Downloads")
  rcb_raw <- get_psrc_pums(5,year,"h",c("PRACE","TEN","GRPIP","HINCP"))
  setwd("J:/Projects/V2050/Housing/Monitoring/2023Update")
  rcb <- rcb_raw
  
  # Filter to only renters, create income/rent burden groupings, rename race/ethnicity categories, combine Some other Race & Two or More Races
  rcb <- rcb %>% filter(TEN=="Rented") %>%
    mutate(
      income_bin=factor(case_when(HINCP < 25000 ~ "Under $25,000",
                                  HINCP < 35000 ~ "$25,000-$34,999",
                                  HINCP < 50000 ~ "$35,000-$49,999",
                                  HINCP < 75000 ~ "$50,000-$74,999",
                                  HINCP < 100000 ~ "$75,000-$99,999",
                                  HINCP >=100000 ~ "$100,000 or more",
                                  !is.na(HINCP) ~ "Else / Prefer not to answer"),
                        levels=c("Under $25,000",
                                 "$25,000-$34,999",                                     
                                 "$35,000-$49,999",
                                 "$50,000-$74,999",
                                 "$75,000-$99,999",
                                 "$100,000 or more",
                                 "Else / Prefer not to answer")),
      rent_burden=factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                   between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                                   GRPIP > 50 ~ "Greater than 50 percent",
                                   !is.na(GRPIP) ~ "No rent paid"),
                         levels=c("Greater than 50 percent",
                                  "Between 30 and 50 percent",
                                  "Less than 30 percent",
                                  "No rent paid")))
  
  # Summarize
  rcb_inc <- psrc_pums_count(rcb, group_vars = c("income_bin","rent_burden"),rr=TRUE)
  rcb_inc <- rcb_inc %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe, reliability))
  
  # Clean table
  rcb_inc <- rcb_inc[, c(1,3,4,5,6,8,7,9,10,11,13,12,14,15,16,18,17,24,25,26,28,27)]
  rcb_inc <- rcb_inc[rcb_inc$income_bin !='Total',]
  
  # Clean variable names
  rcb_inc <- rcb_inc %>% rename_all(~stringr::str_replace_all(.,"count_",""))
  rcb_inc <- rcb_inc %>% rename("No rent paid" = "NA")
  rcb_inc <- rcb_inc %>% rename("moe_No rent paid" = "moe_NA")
  rcb_inc <- rcb_inc %>% rename("Share_No rent paid" = "share_NA")
  rcb_inc <- rcb_inc %>% rename("Reliability_No rent paid" = "reliability_NA")

}

# Run rcb function -----------
rcb_inc_all <- map(years, ~rcb_inc_func(.x)) %>%
  reduce(bind_rows)

# graph change over time -----------
library(psrcplot)
library(ggplot2)

# Removes NA rows - ensures the chart works correctly
rcb_inc_all_chart <- rcb_inc_all[!is.na(rcb_inc_all$`Greater than 50 percent`),]
rcb_inc_severe_cb <- interactive_line_chart(rcb_inc_all_chart, "DATA_YEAR", "share_Greater than 50 percent", fill = "income_bin",
                                          title="Change in Severe Cost Burden by Income (50%+ of income)",color="pgnobgy_5")
rcb_inc_severe_cb

rcb_inc_cb <- interactive_line_chart(rcb_inc_all_chart, "DATA_YEAR", "share_Between 30 and 50 percent", fill = "income_bin",
                                   title="Change in Cost Burden by Income (30-50% of income)",color="pgnobgy_5")
rcb_inc_cb

# Exporting tables------------

library(openxlsx)
work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "rcb_re_all")
writeData(work_book, "rcb_re_all", rcb_re)
addWorksheet(work_book, sheetName = "rcb_inc_all")
writeData(work_book, "rcb_inc_all", rcb_inc)
saveWorkbook(work_book, file = "Renter Cost Burden by RE - Burden Category/r_output.xlsx", overwrite = TRUE)