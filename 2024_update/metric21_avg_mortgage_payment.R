# TITLE: Average Mortgage Payment
# GEOGRAPHIES: King, Snohomish, Pierce (limited by Zillow)
# DATA SOURCE: FreddieMac, Zillow
# DATE MODIFIED: 4.02.2024
# AUTHOR: Eric Clute

library(openxlsx)
library(tidyr)
library(tidyverse)
library(stringr)
library(dplyr)
library(psrccensus)
library(magrittr)
library(ggplot2)

# assumptions
export_path <- "J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric21_avg_mortgage_payment"
source_info <- c("Source: Zillow, Home Value Index; Freddie Mac, Primary Mortgage Market Survey; calculated by PSRC.")

metro_area <- "Seattle, WA"

earliestdate <- "2012-06-30"
latestdate <- "2024-06-30"

smalltbl_earliestdate <- "2021-06-30"
smalltbl_latestdate <- "2024-06-30"

term <- 360                     # 30 year mortgage
downpayment <- 0.20             # JCHS report used 3.5% but I will use 20% given our market conditions
propertytax <- 0.01             # King County is 1%, Snohomish County is 0.89%
propertyins <- 0.0035           # same as JCHS         
mortgageins <- 0.00             # set to 0 for 20% downpayment         
maxdebttoincome <- 0.31         # same as JCHS   

interest_url <- "https://www.freddiemac.com/pmms/docs/historicalweeklydata.xlsx"
ZHVI_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
ZORI_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_sa_month.csv?t=1711667054"

# ZORI: Zillow Observed Rent Index - All Homes + Multifamily, Smoothed, Seasonally-Adjusted
# ZHVI: Zillow Home Value Index - All Homes (SFR & Condo) Time series, Smoothed, Seasonally-Adjusted

# ---------------- Mortgage Rate DATA ----------------
# download Mortgage Rate data from FreddieMac site
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

# ---------------- ZILLOW DATA ----------------
ZORI_raw = read.csv(ZORI_url)
ZHVI_raw = read.csv(ZHVI_url)

# Clean data
ZORI <- subset(ZORI_raw, ZORI_raw$RegionName == metro_area)
ZHVI <- subset(ZHVI_raw, ZHVI_raw$RegionName == metro_area)

colnames(ZORI)<-gsub("X","",colnames(ZORI))
colnames(ZHVI)<-gsub("X","",colnames(ZHVI))

ZORI_long <- ZORI %>% select(starts_with("20"))
ZORI_long %<>% pivot_longer(everything(), names_to = "date", values_to = "zori_rent")
ZORI_long$month <- str_sub(ZORI_long$date, 1, 7)
ZORI_long$month <- gsub("\\.", "-", ZORI_long$month)
ZORI_long  %<>% relocate(month, .before = date)
ZORI_long$date <- gsub("\\.", "-", ZORI_long$date)

ZHVI_long <- ZHVI %>% select(starts_with("20"))
ZHVI_long %<>% pivot_longer(everything(), names_to = "date", values_to = "zhvi_value")
ZHVI_long$month <- str_sub(ZHVI_long$date, 1, 7)
ZHVI_long$month <- gsub("\\.", "-", ZHVI_long$month)
ZHVI_long  %<>% relocate(month, .before = date)
ZHVI_long <- subset(ZHVI_long, select = -c(date))

zillow <- merge(ZHVI_long, ZORI_long, by= "month")

# ---------------- JOIN DATA & ANALYZE ----------------
# crunch monthly payment, required income
analysis <- zillow %>% left_join(int)
analysis <- na.omit(analysis)
analysis$date <- as.Date(analysis$date)

analysis$mthlyrate <- analysis$int_rate / 100 / 12
analysis$r <- (1 + analysis$mthlyrate) ^ term - 1
analysis$loan_amt = analysis$zhvi_value - (downpayment * analysis$zhvi_value)

analysis$propertytax_mnthlypymt = analysis$loan_amt * propertytax / 12
analysis$propertyins_mnthlypymt = analysis$loan_amt * propertyins / 12
analysis$mortgageins_mnthlypymt = analysis$loan_amt * mortgageins / 12

analysis$payment = analysis$loan_amt * analysis$mthlyrate * ((analysis$r + 1) / analysis$r) + (analysis$propertytax_mnthlypymt + analysis$propertyins_mnthlypymt + analysis$mortgageins_mnthlypymt)
analysis$reqincome = (analysis$payment / maxdebttoincome) * 12

# ---------------- SMALL TABLE FOR EXPORT ----------------
smalltbl <- with(analysis, analysis[(date >= smalltbl_earliestdate & date <= smalltbl_latestdate), ])
smalltbl <- subset(smalltbl, str_sub(smalltbl$date, -5,-4) == str_sub(smalltbl_earliestdate, -5,-4))
smalltbl <- subset(smalltbl, select = c(date, int_rate, zhvi_value, payment, reqincome))
smalltbl <- smalltbl %>% arrange(ymd(smalltbl$date))

# ---------------- GRAPHING ----------------
reqincome_vs_int_plot <- ggplot(analysis)  + 
  geom_bar(aes(x=date, y=int_rate*20000),stat="identity", fill="skyblue2",colour="#ffffff")+
  geom_line(aes(x=date, y=reqincome),stat="identity",color="grey40", linewidth=1)+
  labs(title= paste(metro_area, "MSA - Mortgage for Median Home"),
       x="Year",y="Minimum Income Required ($)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),sec.axis=sec_axis(~.*0.00005,name="Mortgage Rate (%)")) +
  scale_x_date(breaks = scales::breaks_width("1 year"),minor_breaks = scales::breaks_width("1 year")) +
  theme(text = element_text(size = 20)) 
reqincome_vs_int_plot

mortgage_vs_rent_plot <- ggplot(analysis)  + 
  geom_line(aes(x=date, y=payment),stat="identity",color="grey40", linewidth=1)+
  geom_line(aes(x=date, y=zori_rent),stat="identity",color="skyblue2", linewidth=1)+
  labs(title= paste(metro_area, "MSA - Mortgage vs Median Rent"),
       x="Year",y="Monthly Payment ($)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),expand = c(0, 0), limits = c(0, NA)) +
  scale_x_date(breaks = scales::breaks_width("1 year"),minor_breaks = scales::breaks_width("1 year")) +
  theme(text = element_text(size = 20)) 
mortgage_vs_rent_plot

# pymt_int_plot <- ggplot(analysis)  + 
#   geom_bar(aes(x=date, y=payment),stat="identity", fill="skyblue2",colour="#ffffff")+
#   geom_line(aes(x=date, y=int_rate*1000),stat="identity",color="grey40", size=1)+
#   labs(title= paste(metro_area, " - Mortgage for Median Home"),
#        x="Year",y="Monthly Mortgage") +
#   scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),sec.axis=sec_axis(~.*0.001,name="Mortgage Rate (%)")) +
#   scale_x_date(breaks = scales::breaks_width("2 year")) +
#   theme(text = element_text(size = 20)) 
# pymt_int_plot

# reqincome_plot <- ggplot(analysis)  + 
#   geom_line(aes(x=date, y=reqincome),stat="identity",color="grey40", size=1)+
#   labs(title= paste(metro_area, " - Mortgage for Median Home"),
#        x="Year",y="Minimum Income Required($)") +
#   scale_x_date(breaks = scales::breaks_width("2 year")) +
#   scale_y_continuous(limits = c(0,200000),labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
#   theme(text = element_text(size = 20)) 
# reqincome_plot

# Export
export_file <- paste0(export_path, "/metric21_raw.xlsx")
work_book <- createWorkbook()

# Add the "by_rgc" sheet
addWorksheet(work_book, sheetName = "analysis")
writeData(work_book, sheet = "analysis", analysis)
writeData(work_book, sheet = "analysis", x = data.frame(source_info), startRow = nrow(analysis) + 3, startCol = 1)

saveWorkbook(work_book, file = export_file, overwrite = TRUE)