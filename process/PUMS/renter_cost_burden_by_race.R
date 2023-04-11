# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS 5YR
# DATE MODIFIED: 3.27.2023
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
rcb_re <- rcb_re %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe))

# Numeric table
rcb_re_num <- rcb_re[, c(3,4,5,6,8,7,9,10,11,13,12)]

# Clean variable names
rcb_re_num <- rcb_re_num %>% rename_all(~stringr::str_replace_all(.,"count_",""))
rcb_re_num <- rcb_re_num %>% rename("No rent paid" = "NA")
rcb_re_num <- rcb_re_num %>% rename("moe_No rent paid" = "moe_NA")

# Percentage table
rcb_re_perc <- rcb_re[, c(3,14,15,16,18,17,19,20,21,23,22)]

# Clean variable names
rcb_re_perc <- rcb_re_perc %>% rename_all(~stringr::str_replace_all(.,"share_",""))
rcb_re_perc <- rcb_re_perc %>% rename("No rent paid" = "NA")
rcb_re_perc <- rcb_re_perc %>% rename("moe_No rent paid" = "moe_NA")


# ----------------------------- SUMMARIZE BY COST BURDEN CATEGORY -----------------------------

# Summarize
rcb_cat <- psrc_pums_count(rcb, group_vars = c("income_bin","rent_burden"))
rcb_cat <- rcb_cat %>% pivot_wider(names_from = rent_burden, values_from = c(count, count_moe, share, share_moe))

# Numeric table
rcb_cat_num <- rcb_cat[, c(3,4,5,6,8,7,9,10,11,13,12)]

# Clean variable names
rcb_cat_num <- rcb_cat_num %>% rename_all(~stringr::str_replace_all(.,"count_",""))
rcb_cat_num <- rcb_cat_num %>% rename("No rent paid" = "NA")
rcb_cat_num <- rcb_cat_num %>% rename("moe_No rent paid" = "moe_NA")

# Percentage table
rcb_cat_perc <- rcb_cat[, c(3,14,15,16,18,17,19,20,21,23,22)]

# Clean variable names
rcb_cat_perc <- rcb_cat_perc %>% rename_all(~stringr::str_replace_all(.,"share_",""))
rcb_cat_perc <- rcb_cat_perc %>% rename("No rent paid" = "NA")
rcb_cat_perc <- rcb_cat_perc %>% rename("moe_No rent paid" = "moe_NA")

# Exporting tables------------

library(openxlsx)

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "rcb_by_RE numeric")
writeData(work_book, "rcb_by_RE numeric", rcb_re_num)
addWorksheet(work_book, sheetName = "rcb_by_RE percent")
writeData(work_book, "rcb_by_RE percent", rcb_re_perc)
addWorksheet(work_book, sheetName = "rcb_by_income numeric")
writeData(work_book, "rcb_by_income numeric", rcb_cat_num)
addWorksheet(work_book, sheetName = "rcb_by_income percent")
writeData(work_book, "rcb_by_income percent", rcb_cat_perc)
saveWorkbook(work_book, file = "Renter Cost Burden by RE - Burden Category/r_output 2021 5YR.xlsx", overwrite = TRUE)
