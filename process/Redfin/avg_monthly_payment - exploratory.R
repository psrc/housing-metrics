# TITLE: Average Mortgage Payment - Exploratory
# GEOGRAPHIES: King and Snohomish Only (limited by Redfin)
# DATA SOURCE: Redfin, FreddieMac, Zillow
# DATE MODIFIED: 8.21.2023
# AUTHOR: Eric Clute

#------------- NOTE --------------
# Use with caution! Here I compare home value data from Redfin (Seattle MD) to rent data from Zillor (Seattle MSA).
# Not compatible geographies. Keeping this file for reference. 

library(openxlsx)
library(tidyr)
library(tidyverse)
library(stringr)
library(dplyr)
library(psrccensus)
library(magrittr)

# assumptions
metro_area <- "Seattle, WA"

earliestdate <- "2012-06-01"
latestdate <- "2023-06-30"

smalltbl_earliestdate <- "2021-06-01"
smalltbl_latestdate <- "2023-06-30"

term <- 360                     # 30 year mortgage
downpayment <- 0.20             # JCHS report used 3.5% but I will use 20% given our market conditions
propertytax <- 0.01             # King County is 1%, Snohomish County is 0.89%
propertyins <- 0.0035           # same as JCHS         
mortgageins <- 0.00             # set to 0 for 20% downpayment         
maxdebttoincome <- 0.31         # same as JCHS   

interest_url <- "https://www.freddiemac.com/pmms/docs/historicalweeklydata.xlsx"
value_url <- "https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/redfin_metro_market_tracker.tsv000.gz"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_sa_month.csv?t=1691785225"

# ---------------- INTEREST RATE DATA ----------------
# download interest rate data from FreddieMac site
int_raw = read.xlsx(interest_url, sheet=1)

# clean up table
int <- int_raw[-(1:4),]
int <- subset(int, select = c(X1, X2))

# clean up data
int$X1 <- convertToDate(int$X1)
int$X2 <- as.numeric(int$X2)
int <- int[!is.na(int$X2),]

int <- int %>% 
  rename("int_date" = "X1") %>%
  rename("int_rate" = "X2")

# refine dates
int <- with(int, int[(int_date >= earliestdate & int_date <= latestdate), ])
int$month <- str_sub(int$int_date, 1, 7)
int <- int[!duplicated(int$month), ] 

# ---------------- REDFIN DATA ----------------
redfin_raw <- read_tsv(value_url)

# Limited to Metro area selected above
value <- redfin_raw %>%
  filter(str_detect(region, metro_area)) %>%
  transmute(
    date = period_begin,
    region = region,
    property_type = property_type,
    median_sale_price = median_sale_price
  )

# limit to all residential properties, restrict to date range selected above
value <- value %>%
  filter(property_type == "All Residential")
value <- with(value, value[(date >= earliestdate & date <= latestdate), ])
value$month <- str_sub(value$date, 1, 7)

# ---------------- REDFIN DATA ----------------
#  ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted
ZORI_raw = read.csv(ZORI_url)

# Clean data
ZORI <- subset(ZORI_raw, ZORI_raw$RegionName == metro_area)
colnames(ZORI)<-gsub("X","",colnames(ZORI))

ZORI_long <- ZORI %>% dplyr::select(starts_with("20"))
ZORI_long %<>% pivot_longer(everything(), names_to = "zori_date", values_to = "zori_rent")

ZORI_long$month <- str_sub(ZORI_long$zori_date, 1, 7)
ZORI_long$month <- gsub("\\.", "-", ZORI_long$month)

# ---------------- JOIN DATA & ANALYZE ----------------
# crunch monthly payment, required income
analysis <- value %>% left_join(int)

analysis$mthlyrate <- analysis$int_rate / 100 / 12
analysis$r <- (1 + analysis$mthlyrate) ^ term - 1
analysis$loan_amt = analysis$median_sale_price - (downpayment * analysis$median_sale_price)

analysis$propertytax_mnthlypymt = analysis$loan_amt * propertytax / 12
analysis$propertyins_mnthlypymt = analysis$loan_amt * propertyins / 12
analysis$mortgageins_mnthlypymt = analysis$loan_amt * mortgageins / 12

analysis$payment = analysis$loan_amt * analysis$mthlyrate * ((analysis$r + 1) / analysis$r) + (analysis$propertytax_mnthlypymt + analysis$propertyins_mnthlypymt + analysis$mortgageins_mnthlypymt)
analysis$reqincome = (analysis$payment / maxdebttoincome) * 12

# compare to median asking rent (Zillow)
analysis <- analysis %>% left_join(ZORI_long)

# ---------------- SMALL TABLE FOR EXPORT ----------------
smalltbl <- with(analysis, analysis[(date >= smalltbl_earliestdate & date <= smalltbl_latestdate), ])
smalltbl <- subset(smalltbl, str_sub(smalltbl$date, -5,-1) == str_sub(smalltbl_earliestdate, -5,-1))
smalltbl <- subset(smalltbl, select = c(date, int_rate, median_sale_price, payment, reqincome))
smalltbl <- smalltbl %>% arrange(ymd(smalltbl$date))
smalltbl <- t(smalltbl)

# ---------------- GRAPHING ----------------
library(ggplot2)

# pymt_int_plot <- ggplot(analysis)  + 
#   geom_bar(aes(x=date, y=payment),stat="identity", fill="skyblue2",colour="#ffffff")+
#   geom_line(aes(x=date, y=int_rate*1000),stat="identity",color="grey40", size=1)+
#   labs(title= paste(metro_area, " - Mortgage for Median Home"),
#        x="Year",y="Monthly Mortgage") +
#   scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),sec.axis=sec_axis(~.*0.001,name="Interest Rate (%)")) +
#   scale_x_date(breaks = scales::breaks_width("2 year")) +
#   theme(text = element_text(size = 20)) 
# pymt_int_plot

reqincome_vs_int_plot <- ggplot(analysis)  + 
  geom_bar(aes(x=date, y=int_rate*20000),stat="identity", fill="skyblue2",colour="#ffffff")+
  geom_line(aes(x=date, y=reqincome),stat="identity",color="grey40", size=1)+
  labs(title= paste(metro_area, " - Mortgage for Median Home"),
       x="Year",y="Minimum Income Required ($)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),sec.axis=sec_axis(~.*0.00005,name="Interest Rate (%)")) +
  scale_x_date(breaks = scales::breaks_width("2 year")) +
  theme(text = element_text(size = 20)) 
reqincome_vs_int_plot

# reqincome_plot <- ggplot(analysis)  + 
#   geom_line(aes(x=date, y=reqincome),stat="identity",color="grey40", size=1)+
#   labs(title= paste(metro_area, " - Mortgage for Median Home"),
#        x="Year",y="Minimum Income Required($)") +
#   scale_x_date(breaks = scales::breaks_width("2 year")) +
#   scale_y_continuous(limits = c(0,200000),labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
#   theme(text = element_text(size = 20)) 
# reqincome_plot

mortgage_vs_rent_plot <- ggplot(analysis)  + 
  geom_line(aes(x=date, y=payment),stat="identity",color="grey40", size=1)+
  geom_line(aes(x=date, y=zori_rent),stat="identity",color="skyblue2", size=1)+
  labs(title= paste(metro_area, " - Mortgage vs Median Rent"),
       x="Year",y="Monthly Payment") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(breaks = scales::breaks_width("2 year")) +
  theme(text = element_text(size = 20)) 
mortgage_vs_rent_plot
