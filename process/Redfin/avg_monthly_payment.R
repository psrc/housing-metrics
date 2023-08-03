# TITLE: Average Mortgage Payment
# GEOGRAPHIES: King and Snohomish Only (limited by Redfin)
# DATA SOURCE: Redfin, FreddieMac
# DATE MODIFIED: 8.3.2023
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(tidyverse)
library(stringr)
library(dplyr)

# assumptions
earliestdate <- "2012-06-01"
latestdate <- "2023-06-30"

term <- 360                     # 30 year mortgage
downpayment <- 0.035            # same as JCHS State of The Nation's Housing 2023 Report
propertytax <- 0.01             # King County is 1%, Snohomish County is 0.89%
propertyins <- 0.0035           # same as JCHS         
mortgageins <- 0.0085           # same as JCHS         
maxdebttoincome <- 0.31         # same as JCHS   

interest_url <- "https://www.freddiemac.com/pmms/docs/historicalweeklydata.xlsx"
value_url <- "https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/redfin_metro_market_tracker.tsv000.gz"

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

# Seattle Metro area only includes King and Snohomish Counties
value <- redfin_raw %>%
  filter(region == "Seattle, WA metro area") %>%
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
value <- subset(value, select = c(date,median_sale_price,property_type))
value$month <- str_sub(value$date, 1, 7)

# ---------------- JOIN DATA & ANALYZE ----------------

analysis <- value %>% left_join(int)

analysis$mthlyrate <- analysis$int_rate / 100 / 12
analysis$r <- (1 + analysis$mthlyrate) ^ term - 1
analysis$loan_amt = analysis$median_sale_price - (downpayment * analysis$median_sale_price)

analysis$propertytax_mnthlypymt = analysis$loan_amt * propertytax / 12
analysis$propertyins_mnthlypymt = analysis$loan_amt * propertyins / 12
analysis$mortgageins_mnthlypymt = analysis$loan_amt * mortgageins / 12

analysis$payment = analysis$loan_amt * analysis$mthlyrate * ((analysis$r + 1) / analysis$r) + (analysis$propertytax_mnthlypymt + analysis$propertyins_mnthlypymt + analysis$mortgageins_mnthlypymt)
analysis$reqincome = (analysis$payment / maxdebttoincome) * 12

# ---------------- GRAPHING ----------------
library(ggplot2)

pymt_int_plot <- ggplot(analysis)  + 
  geom_bar(aes(x=date, y=payment),stat="identity", fill="skyblue2",colour="#ffffff")+
  geom_line(aes(x=date, y=int_rate*1000),stat="identity",color="grey40", size=1)+
  labs(title= "Average Monthly Mortgage vs. Interest Rates",
       x="Year",y="Monthly Mortgage") +
  scale_y_continuous(sec.axis=sec_axis(~.*0.001,name="Interest Rate (%)")) +
  scale_x_date(breaks = scales::breaks_width("2 year")) +
  theme(text = element_text(size = 20)) 
pymt_int_plot


reqincome_plot <- ggplot(analysis)  + 
  geom_line(aes(x=date, y=reqincome),stat="identity",color="grey40", size=1)+
  labs(title= "Income Required to Buy Median Home",
       x="Year",y="Income ($)") +
  scale_x_date(breaks = scales::breaks_width("2 year")) +
  theme(text = element_text(size = 20)) 
reqincome_plot
