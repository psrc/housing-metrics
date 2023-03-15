# TITLE: Renter Cost Burden by Race
# GEOGRAPHIES: PSRC Region
# DATA SOURCE: ACS PUMS
# DATE MODIFIED: 3.14.2023
# AUTHOR: Eric Clute

library(magrittr)
library(psrccensus)
library(dplyr)
library(srvyr)

rcb_raw <- get_psrc_pums(5,2021,"h",c("PRACE","TEN","GRPIP","HINCP"))
rcb <- rcb_raw
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
                                         !is.na(GRPIP) ~ "Else"),
                                levels=c("Greater than 50 percent",
                                         "Between 30 and 50 percent",
                                         "Less than 30 percent", "Else")))
inc_rb_rgn <- psrc_pums_count(rcb, group_vars=c("PRACE","income_bin","rent_burden"))
