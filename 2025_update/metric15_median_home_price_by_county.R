# TITLE: Median Sale Price by County
# GEOGRAPHIES: All four counties
# DATA SOURCE: Redfin (https://public.tableau.com/shared/GRN635HN7?:display_count=n&:origin=viz_share_link)
# DATE MODIFIED: 7.16.2025
# AUTHOR: Eric Clute

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

data_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric15_median_home_price_by_county/data_redfin.csv"
save_path <- "J:/Projects/V2050/Housing/Monitoring/2025Update/data/metric15_median_home_price_by_county/metric15_raw.csv"
month <- "June"

# Pull in data downloaded from tableau dashboard (make sure all 4 counties are visible before exporting to CSV)
county_raw <- read_tsv(file = data_path, locale = locale(encoding = "UTF-16LE"))

# Clean
colnames(county_raw) <- as.character(county_raw[1, ])
county_raw <- county_raw[-1, ]

county <- county_raw %>%
  select(1, starts_with(month)) %>%
  mutate(across(
    .cols = -1,                               # Apply to all columns except the first (region)
    .fns = ~ as.numeric(
      str_replace_all(.x, "\\$", "") |>       # Remove $
        str_replace_all("K", "000")           # Replace K with 000
    )))

# Export
write.csv(county, file = save_path)
