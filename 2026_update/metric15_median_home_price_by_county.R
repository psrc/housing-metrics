# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Zillow (https://www.zillow.com/research/data/)
# DATE MODIFIED: 8.10.2026
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

data_path <- "J:/Projects/V2050/Housing/Monitoring/2026Update/data/metric15_median_home_price_by_county/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
save_path <- "J:/Projects/V2050/Housing/Monitoring/2026Update/data/metric15_median_home_price_by_county/metric15_raw.csv"

# Pull data
raw_zhvi <- read.csv(data_path) %>%
  filter(State == "WA") %>%
  filter(RegionName %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County"))%>%
  arrange(RegionName) #sort aphabetically

# Clean
cleaned_zhvi <- raw_zhvi %>% pivot_longer(cols = starts_with("X"), names_to = "Month", values_to = "Value") %>%
                          select(RegionName, Month, Value) %>%
                          pivot_wider(names_from = RegionName, values_from = Value) %>%
                          filter(grepl("06.30$", Month)) %>% 
                          mutate(Month = sub("^X", "", Month))

# Export
write.csv(cleaned_zhvi, file = save_path)
