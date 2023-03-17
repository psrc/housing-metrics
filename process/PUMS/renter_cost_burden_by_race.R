# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS
# DATE MODIFIED: 3.14.2023
# AUTHOR: Eric Clute

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)

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
inc_rb_rgn <- psrc_pums_count(rcb, group_vars=c("PRACE","income_bin","rent_burden"))

# SUMMARIZE BY RACE/ETHNICITY -----------------------------
# Summarize
rcb_by_re <- inc_rb_rgn %>% group_by(PRACE,rent_burden) %>% summarize(renters = sum(count))
rcb_by_re <- rcb_by_re %>% pivot_wider(names_from = rent_burden, values_from = renters)

# Rename and rearrange columns
rcb_by_re <- rcb_by_re %>% rename("No Rent Paid" = "NA")
rcb_by_re <- rcb_by_re[, c(1,2,3,4,6,5)]

# SUMMARIZE BY COST BURDEN -----------------------------
# Summarize
rcb_by_cat <- inc_rb_rgn %>% group_by(income_bin,rent_burden) %>% summarize(renters = sum(count))
rcb_by_cat <- rcb_by_cat %>% pivot_wider(names_from = rent_burden, values_from = renters)

# Rename and rearrange columns
rcb_by_cat <- rcb_by_cat %>% rename("No Rent Paid" = "NA")
rcb_by_cat <- rcb_by_cat[, c(1,2,3,4,6,5)]

# Exporting table

library(openxlsx)

work_book <- createWorkbook()
addWorksheet(work_book, sheetName = "RenterCostBurdenbyRE")
writeData(work_book, "RenterCostBurdenbyRE", inc_rb_rgn)
saveWorkbook(work_book, file = "Renter Cost Burden by RE\renter_cost_burden_by_RE.xlsx", overwrite = TRUE)
