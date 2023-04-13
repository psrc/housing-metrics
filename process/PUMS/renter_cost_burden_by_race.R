# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS 5YR
# DATE MODIFIED: 4.13.2023
# AUTHOR: Eric Clute

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)
library(tidyr)

# Pull PUMS data
setwd("C:/Users/eclute/Downloads")
rcb_raw <- get_psrc_pums(5,2021,"h",c("PRACE","TEN","GRPIP","HINCP"))
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
                                       "No rent paid")),
          PRACE=factor(
            case_when(grepl("Other Race|Two or More Races", PRACE) ~"Other or Multiple Races",
                      grepl("^Black ", PRACE) ~"Black",
                      grepl("^Hispanic ", PRACE) ~"Hispanic/Latinx",
                      !is.na(PRACE) ~stringr::str_replace_all(as.character(PRACE), " (and|or) ", "/") %>%
                        stringr::str_replace(" alone", "") %>%
                        stringr::str_replace(" Alone", ""))))

# ----------------------------- SUMMARIZE BY RACE/ETHNICITY -----------------------------
# Summarize
rcb_re <- psrc_pums_count(rcb, group_vars = c("PRACE","rent_burden"),rr=TRUE)
rcb_re <- rcb_re %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe, reliability))

# Clean table
rcb_re <- rcb_re[, c(3,4,5,6,8,7,9,10,11,13,12,14,15,16,18,17,24,25,26,28,27)]

# Clean variable names
rcb_re <- rcb_re %>% rename_all(~stringr::str_replace_all(.,"count_",""))
rcb_re <- rcb_re %>% rename("No rent paid" = "NA")
rcb_re <- rcb_re %>% rename("moe_No rent paid" = "moe_NA")
rcb_re <- rcb_re %>% rename("Share_No rent paid" = "share_NA")
rcb_re <- rcb_re %>% rename("Reliability_No rent paid" = "reliability_NA")

# ----------------------------- SUMMARIZE BY COST BURDEN CATEGORY -----------------------------

# Summarize
rcb_cat <- psrc_pums_count(rcb, group_vars = c("income_bin","rent_burden"),rr=TRUE)
rcb_cat <- rcb_cat %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe, reliability))

# Clean table
rcb_cat <- rcb_cat[, c(3,4,5,6,8,7,9,10,11,13,12,14,15,16,18,17,24,25,26,28,27)]

# Clean variable names
rcb_cat <- rcb_cat %>% rename_all(~stringr::str_replace_all(.,"count_",""))
rcb_cat <- rcb_cat %>% rename("No rent paid" = "NA")
rcb_cat <- rcb_cat %>% rename("Share_No rent paid" = "share_NA")
rcb_cat <- rcb_cat %>% rename("Reliability_No rent paid" = "reliability_NA")

# Exporting tables------------

library(openxlsx)
work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "rcb_by_RE")
writeData(work_book, "rcb_by_RE", rcb_re)
addWorksheet(work_book, sheetName = "rcb_by_income")
writeData(work_book, "rcb_by_income", rcb_cat)
saveWorkbook(work_book, file = "Renter Cost Burden by RE - Burden Category/r_output 2021 5YR.xlsx", overwrite = TRUE)