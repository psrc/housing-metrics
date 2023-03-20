# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS
# DATE MODIFIED: 3.20.2023
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

# Filter to only renters, create income/rent burden groupings
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
                                         !is.na(GRPIP) ~ "No Rent Charged"),
                                levels=c("Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent", "No Rent Charged")))

# Create full table
rcb <- psrc_pums_count(rcb, group_vars=c("PRACE","income_bin","rent_burden"))

# Modify Race/Ethnicity Categories
rcb$PRACE <- gsub("American Indian or Alaskan Native Alone", "American Indian/Alaskan Native", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("Asian alone", "Asian", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("Black or African American alone", "Black", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("Hispanic or Latino", "Hispanic/Latinx", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("Native Hawaiian and Other Pacific Islander alone", "Native Hawaiian/Other Pacific Islander", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("Some Other Race alone", "Other Race", rcb$PRACE, ignore.case = TRUE)
rcb$PRACE <- gsub("White alone", "White", rcb$PRACE, ignore.case = TRUE)

# ----------------------------- SUMMARIZE BY RACE/ETHNICITY -----------------------------
# Summarize
rcb_re <- rcb %>% group_by(PRACE,rent_burden) %>% summarize(renters = sum(count))
rcb_re <- rcb_re %>% pivot_wider(names_from = rent_burden, values_from = renters)

# Rename, rearrange, and recalculate columns
rcb_re <- rcb_re %>% rename("No rent paid" = "NA")
rcb_re <- rcb_re %>% mutate(`No rent paid` = na_if(`No rent paid`, 0))
rcb_re$Total <- rowSums(rcb_re[,c("Greater than 50 percent", "Between 30 and 50 percent", "Less than 30 percent", "No rent paid")], na.rm=TRUE)
rcb_re <- rcb_re[, c(1,2,3,4,6,5)]

# Sum variables for Other Race and Two or More Races - Becoming "Other Race/Two or More Races"
temp <- subset(rcb_re, rcb_re$PRACE == "Other Race" | rcb_re$PRACE == "Two or More Races")


# Add combined row to original data frame
rcb_re <- rbind(rcb_re, temp)

# Create percentage output
rcb_re_perc <- rcb_re
rcb_re_perc$`Severely cost burdened` <- rcb_re_perc$`Greater than 50 percent`/rcb_re_perc$Total
rcb_re_perc$`Cost burdened` <- rcb_re_perc$`Between 30 and 50 percent`/rcb_re_perc$Total
rcb_re_perc$`Not cost burdened` <- rcb_re_perc$`Less than 30 percent`/rcb_re_perc$Total
rcb_re_perc$`No income or no rent paid` <- rcb_re_perc$`No rent paid`/rcb_re_perc$Total

rcb_re_perc <- rcb_re_perc[, c(1,7,8,9,10)]

# ----------------------------- SUMMARIZE BY COST BURDEN -----------------------------
# Summarize
rcb_cat <- rcb %>% group_by(income_bin,rent_burden) %>% summarize(renters = sum(count))
rcb_cat <- rcb_cat %>% pivot_wider(names_from = rent_burden, values_from = renters)

# Rename and rearrange columns
rcb_cat <- rcb_cat %>% rename("No rent paid" = "NA")
rcb_cat$Total <- rowSums(rcb_cat[,c("Greater than 50 percent", "Between 30 and 50 percent", "Less than 30 percent", "No rent paid")], na.rm=TRUE)
rcb_cat <- rcb_cat[, c(1,2,3,4,6,5)]

# Create percentage output
rcb_cat_perc <- rcb_cat
rcb_cat_perc$`Severely cost burdened` <- rcb_cat_perc$`Greater than 50 percent`/rcb_cat_perc$Total
rcb_cat_perc$`Cost burdened` <- rcb_cat_perc$`Between 30 and 50 percent`/rcb_cat_perc$Total
rcb_cat_perc$`Not cost burdened` <- rcb_cat_perc$`Less than 30 percent`/rcb_cat_perc$Total
rcb_cat_perc$`No income or no rent paid` <- rcb_cat_perc$`No rent paid`/rcb_cat_perc$Total

rcb_cat_perc <- rcb_cat_perc[, c(1,7,8,9,10)]

# Exporting tables

library(openxlsx)

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "CostBurdenbyRE")
writeData(work_book, "CostBurdenbyRE", rcb_re)
addWorksheet(work_book, sheetName = "CostBurdenbyRE_perc")
writeData(work_book, "CostBurdenbyRE_perc", rcb_re_perc)
addWorksheet(work_book, sheetName = "CostBurdenbyCategory")
writeData(work_book, "CostBurdenbyCategory", rcb_cat)
addWorksheet(work_book, sheetName = "CostBurdenbyCat_perc")
writeData(work_book, "CostBurdenbyCat_perc", rcb_cat_perc)
saveWorkbook(work_book, file = "Renter Cost Burden by RE/renter_cost_burden.xlsx", overwrite = TRUE)
