# TITLE: Ownership Opportunities in Centers and Near HCT
# GEOGRAPHIES: PSRC Region and Counties
# DATA SOURCE: ACS
# DATE: 5.30.2024
# AUTHOR: Kristin Mitchell


library(psrccensus)
library(psrcelmer)
library(psrcplot)
library(tidyverse)
library(stringr)
library(dplyr)

# Inputs ------------------------------------------------------------------
years <- c(2017,2021,2022)
county_order <- c("King County","Kitsap County", "Pierce County", "Snohomish County", "Region Total")
value_order <- c("Total", "Less than $300,000","$300,000 to $499,999","$500,000 to $749,999","$750,000 to $999,999","$1,000,000 to $1,499,999", 
                 "$1,500,000 to $1,999,999", "$2,000,000 or more")
value_order_old <- c("Total", "Less than $300,000","$300,000 to $499,999","$500,000 to $749,999","$750,000 to $999,999","$1,000,000 or more")
bedrooms_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", "4 bedrooms", "5+ bedrooms", "Total")

# Blockgroup Splits from Elmer --------------------------------------------
hct_blockgroup <- NULL

q10 <- paste0("SELECT * FROM general.get_geography_splits('blockgroup10', 'HCT Station Areas (Vision 2050) and RGCs', 2010, 2020, 2014)")
q17 <- paste0("SELECT * FROM general.get_geography_splits('blockgroup10', 'HCT Station Areas (Vision 2050) and RGCs', 2017, 2020, 2014)")
q21 <- paste0("SELECT * FROM general.get_geography_splits('blockgroup20', 'HCT Station Areas (Vision 2050) and RGCs', 2021, 2022, 2018)")
q22 <- paste0("SELECT * FROM general.get_geography_splits('blockgroup20', 'HCT Station Areas (Vision 2050) and RGCs', 2022, 2022, 2018)")

s10 <- get_query(sql = q10, db_name = "Elmer")
s17 <- get_query(sql = q17, db_name = "Elmer")
s21 <- get_query(sql = q21, db_name = "Elmer")
s22 <- get_query(sql = q22, db_name = "Elmer")

hct_blockgroup <- bind_rows(s10, s17, s21, s22)
rm(q10, q17, q21, q22, s10, s17, s21, s22)

# Housing Tenure ----------------------------------------------------------

# Download ACS data and clean
tenure <- get_acs_recs(geography = 'block group', table.names = c('B25003'), years = years)

tenure <- tenure %>%
  dplyr::select(geoid="GEOID", geography="census_geography", "estimate", "moe", "variable", "year") %>%
  mutate(variable = factor(case_when(variable == "B25003_001" ~ "total households",
                                     variable == "B25003_002" ~ "owner",
                                     variable == "B25003_003" ~ "renter",
                                     !is.na(variable) ~"N/A"),
                           levels=c("total households","owner","renter")))

# Download 2010 ACS data and clean
tenure10 <- read_csv("J:\\Projects\\V2050\\Housing\\Monitoring\\2024Update\\Data\\metric10_ownership_near_transit\\2010_files\\nhgis0001_csv\\B25003\\Tenure-2010-5yr.csv") %>% 
  mutate(geoid = str_remove_all(geoid, "15000US")) %>% 
  dplyr::select("year", "county", "geoid", `estimate-total households`="total households", `estimate-owner`="owner", `estimate-renter`="renter", `moe-total households`="total-moe", `moe-owner`="owner-moe", `moe-renter`="renter-moe") 

tenure10_pivoted <- tenure10 %>% 
  pivot_longer(cols = !c(year, county, geoid), names_to = c(".value", "variable"), names_sep = "-") %>%
  mutate(variable = factor(variable, levels=c("total households","owner","renter")))
  
#Combine 2010 with Other Years

tenure_all <- bind_rows(tenure, tenure10_pivoted) %>% dplyr::select(-"county", -"geography")

# Calculate Estimates using Blockgroup splits from Elmer Parcelization Data 
in_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", inside_split="percent_of_occupied_housing_units")

out_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="not in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", outside_split="percent_of_occupied_housing_units")

tenure_by_hct <- left_join(tenure_all, in_hct_split, by=c("geoid", "year")) %>% 
  mutate(inside_split=replace_na(inside_split, 0)) %>%
  mutate(inside_estimate = round(estimate*inside_split,0))

tenure_by_hct <- left_join(tenure_by_hct, out_hct_split, by=c("geoid", "year")) %>% 
  mutate(outside_split=replace_na(outside_split, 0)) %>%
  mutate(outside_estimate = round(estimate*outside_split,0))

# Add County Name
tenure_by_hct <- tenure_by_hct %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))

# Create Clean Blockgroup Data
tenure_by_hct <- tenure_by_hct %>%
  dplyr::select("geoid","county","variable","year",`Inside centers and HCT station areas`="inside_estimate",`Outside centers and HCT station areas`="outside_estimate") %>%
  pivot_longer(!c(geoid,county,variable,year), names_to = "metric", values_to = "estimate")
  
# Create Summary Chart
tenure_summary <- tenure_by_hct %>%
  group_by(county, year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

region_summary <- tenure_by_hct %>%
  group_by(year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

tenure_summary <- bind_rows(tenure_summary, region_summary) %>%
  mutate(county = factor(county, levels=county_order))

totals <- tenure_summary %>% filter(variable=="total households") %>% rename(total=estimate) %>% select(-"variable")

tenure_summary <- left_join(tenure_summary, totals, by=c("county","year", "metric")) %>%
  mutate(share=estimate/total) %>%
  select(-"total")

tenure_chart_2010 <- static_column_chart(t = tenure_summary %>% filter(year==2010 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Ownership Housing in Centers and Near Transit: 2010",
                                         source="Source: US Census Bureau 2006-2010 American Community Survey 5-year Estimates Table B25003")

tenure_chart_2017 <- static_column_chart(t = tenure_summary %>% filter(year==2017 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Ownership Housing in Centers and Near Transit: 2017",
                                         source="Source: US Census Bureau 2013-2017 American Community Survey 5-year Estimates Table B25003")

tenure_chart_2021 <- static_column_chart(t = tenure_summary %>% filter(year==2021 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Ownership Housing in Centers and Near Transit: 2021",
                                         source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25003")

tenure_chart_2022 <- static_column_chart(t = tenure_summary %>% filter(year==2022 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Ownership Housing in Centers and Near Transit: 2022",
                                         source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25003")

tenure_chart_years <- static_column_chart(t = tenure_summary %>% filter(year==2010 & 2017 & 2022 & variable=="owner"),
                                         x = "year" & "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Ownership Housing in Centers and Near Transit: 2010, 2017, 2022",
                                         source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25003")



rm(in_hct_split, out_hct_split, tenure, totals, region_summary)

write_csv(tenure_summary, file = 'J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric10_ownership_near_transit\\ownershipopps24.csv')

# Home Value --------------------------------------------------------------

# Download ACS data and clean - Updated Value Groups - updated value buckets
value <- get_acs_recs(geography = 'block group', table.names = c('B25075'), years = years)

value <- value %>%
  dplyr::select(geoid="GEOID", geography="census_geography", "estimate", "moe", "variable", "year") %>%
  separate(variable, into=c("table","variable"), sep="_", remove=TRUE) %>%
  mutate(variable = as.integer(variable)) %>%
  mutate(label = case_when(variable == 1 ~ "Total",
                           variable <=20 ~ "Less than $300,000",
                           variable <=22 ~ "$300,000 to $499,999",
                           variable ==23 ~ "$500,000 to $749,999",
                           variable ==24 ~ "$750,000 to $999,999",
                           variable ==25 ~ "$1,000,000 to $1,499,999",
                           variable ==26 ~ "$1,500,000 to $1,999,999",
                           variable ==27 ~ "$2,000,000 or more"
                           )) %>%
  dplyr::select(-"geography",-"table",-"variable", -"moe") %>%
  group_by(geoid, year, label) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  rename(variable="label") %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))


# Download ACS data and clean - 2010
value2010 <- read_csv("J:\\Projects\\V2050\\Housing\\Monitoring\\2024Update\\Data\\metric10_ownership_near_transit\\2010_files\\nhgis0001_csv\\B25075\\value-2010-5yr.csv") %>%
pivot_longer(cols = !c(YEAR, COUNTY, GEOID), names_to = c(".value", "variable"), names_sep = "-") %>%
mutate(variable = factor(variable))


#, levels=c("total households","owner","renter")))

value2010 <- value2010 %>%
  dplyr::select(geoid="GEOID", "estimate", "moe", "variable", "year") %>%
  separate(variable, into=c("table","variable"), sep="_", remove=TRUE) %>%
  mutate(variable = as.integer(variable)) %>%
  mutate(label = case_when(variable == 1 ~ "Total",
                           variable <=14 ~ "Less than $100,000",
                           variable <=18 ~ "$100,000 to $199,999",
                           variable <=20 ~ "$200,000 to $299,999",
                           variable ==21 ~ "$300,000 to $399,999",
                           variable ==22 ~ "$400,000 to $499,999",
                           variable ==23 ~ "$500,000 to $749,999",
                           variable ==24 ~ "$750,000 to $999,999",
                           variable ==25 ~ "$1,000,000 to $1,499,999",
                           variable ==26 ~ "$1,500,000 to $1,999,999",
                           variable ==27 ~ "$2,000,000 or more")) %>%
  dplyr::select(-"geography",-"table",-"variable", -"moe") %>%
  group_by(geoid, year, label) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  rename(variable="label") %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))

# Calculate Estimates using Blockgroup splits from Elmer Parcelization Data 
in_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", inside_split="percent_of_occupied_housing_units")

out_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="not in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", outside_split="percent_of_occupied_housing_units")

value_by_hct <- left_join(value, in_hct_split, by=c("geoid", "year")) %>% 
  mutate(inside_split=replace_na(inside_split, 0)) %>%
  mutate(inside_estimate = round(estimate*inside_split,0))

value_by_hct <- left_join(value_by_hct, out_hct_split, by=c("geoid", "year")) %>% 
  mutate(outside_split=replace_na(outside_split, 0)) %>%
  mutate(outside_estimate = round(estimate*outside_split,0))

# Create Clean Blockgroup Data
value_by_hct <- value_by_hct %>%
  dplyr::select("geoid","county","variable","year",`Inside centers and HCT station areas`="inside_estimate",`Outside centers and HCT station areas`="outside_estimate") %>%
  pivot_longer(!c(geoid,county,variable,year), names_to = "metric", values_to = "estimate")

# Create Summary Chart
value_summary <- value_by_hct %>%
  group_by(county, year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

region_summary <- value_by_hct %>%
  group_by(year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

value_summary <- bind_rows(value_summary, region_summary) %>%
  mutate(county = factor(county, levels=county_order)) %>%
  mutate(variable = factor(variable, levels=value_order))

totals <- value_summary %>% filter(variable=="Total") %>% rename(total=estimate) %>%  dplyr::select(-"variable")

value_summary <- left_join(value_summary, totals, by=c("county","year", "metric")) %>%
  mutate(share=estimate/total) %>%
  dplyr::select(-"total")

value_chart_2017 <- static_column_chart(t = value_summary %>% filter(year==2017 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2017",
                                        source="Source: US Census Bureau 2013-2017 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

value_chart_2021 <- static_column_chart(t = value_summary %>% filter(year==2021 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2021",
                                        source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

value_chart_2022 <- static_column_chart(t = value_summary %>% filter(year==2022 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2022",
                                        source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


rm(in_hct_split, out_hct_split, value, totals, region_summary)

write_csv(value_summary, file = 'J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric10_ownership_near_transit\\ownershipvalues2024.csv')

# Download ACS data and clean - Updated Value Groups - old value buckets
value <- get_acs_recs(geography = 'block group', table.names = c('B25075'), years = years)

value <- value %>%
  dplyr::select(geoid="GEOID", geography="census_geography", "estimate", "moe", "variable", "year") %>%
  separate(variable, into=c("table","variable"), sep="_", remove=TRUE) %>%
  mutate(variable = as.integer(variable)) %>%
  mutate(label = case_when(variable == 1 ~ "Total",
                           variable <=20 ~ "Less than $300,000",
                           variable <=22 ~ "$300,000 to $499,999",
                           variable ==23 ~ "$500,000 to $749,999",
                           variable ==24 ~ "$750,000 to $999,999",
                           variable <=27 ~ "$1,000,000 or more"
  )) %>%
  dplyr::select(-"geography",-"table",-"variable", -"moe") %>%
  group_by(geoid, year, label) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  rename(variable="label") %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))


# Download ACS data and clean - 2010
value2010 <- read_csv("J:\\Projects\\V2050\\Housing\\Monitoring\\2024Update\\Data\\metric10_ownership_near_transit\\2010_files\\nhgis0001_csv\\B25075\\value-2010-5yr.csv") %>%
  pivot_longer(cols = !c(YEAR, COUNTY, GEOID), names_to = c(".value", "variable"), names_sep = "-") %>%
  mutate(variable = factor(variable))


#, levels=c("total households","owner","renter")))

value2010 <- value2010 %>%
  dplyr::select(geoid="GEOID", "estimate", "moe", "variable", "year") %>%
  separate(variable, into=c("table","variable"), sep="_", remove=TRUE) %>%
  mutate(variable = as.integer(variable)) %>%
  mutate(label = case_when(variable == 1 ~ "Total",
                           variable <=14 ~ "Less than $100,000",
                           variable <=18 ~ "$100,000 to $199,999",
                           variable <=20 ~ "$200,000 to $299,999",
                           variable ==21 ~ "$300,000 to $399,999",
                           variable ==22 ~ "$400,000 to $499,999",
                           variable ==23 ~ "$500,000 to $749,999",
                           variable ==24 ~ "$750,000 to $999,999",
                           variable ==25 ~ "$1,000,000 to $1,499,999",
                           variable ==26 ~ "$1,500,000 to $1,999,999",
                           variable ==27 ~ "$2,000,000 or more")) %>%
  dplyr::select(-"geography",-"table",-"variable", -"moe") %>%
  group_by(geoid, year, label) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  rename(variable="label") %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))

# Calculate Estimates using Blockgroup splits from Elmer Parcelization Data 
in_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", inside_split="percent_of_occupied_housing_units")

out_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="not in station area or rgc") %>%
  dplyr::select(geoid="data_geog", year="ofm_estimate_year", outside_split="percent_of_occupied_housing_units")

value_by_hct <- left_join(value, in_hct_split, by=c("geoid", "year")) %>% 
  mutate(inside_split=replace_na(inside_split, 0)) %>%
  mutate(inside_estimate = round(estimate*inside_split,0))

value_by_hct <- left_join(value_by_hct, out_hct_split, by=c("geoid", "year")) %>% 
  mutate(outside_split=replace_na(outside_split, 0)) %>%
  mutate(outside_estimate = round(estimate*outside_split,0))

# Create Clean Blockgroup Data
value_by_hct <- value_by_hct %>%
  dplyr::select("geoid","county","variable","year",`Inside centers and HCT station areas`="inside_estimate",`Outside centers and HCT station areas`="outside_estimate") %>%
  pivot_longer(!c(geoid,county,variable,year), names_to = "metric", values_to = "estimate")

# Create Summary Chart
value_summary <- value_by_hct %>%
  group_by(county, year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

region_summary <- value_by_hct %>%
  group_by(year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

value_summary <- bind_rows(value_summary, region_summary) %>%
  mutate(county = factor(county, levels=county_order)) %>%
  mutate(variable = factor(variable, levels=value_order_old))

totals <- value_summary %>% filter(variable=="Total") %>% rename(total=estimate) %>%  dplyr::select(-"variable")

value_summary <- left_join(value_summary, totals, by=c("county","year", "metric")) %>%
  mutate(share=estimate/total) %>%
  dplyr::select(-"total")

value_chart_2017_old <- static_column_chart(t = value_summary %>% filter(year==2017 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2017",
                                        source="Source: US Census Bureau 2013-2017 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

value_chart_2021_old <- static_column_chart(t = value_summary %>% filter(year==2021 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2021",
                                        source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

value_chart_2022_old <- static_column_chart(t = value_summary %>% filter(year==2022 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Home Value in Centers and Near Transit: 2022",
                                        source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25075") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


rm(in_hct_split, out_hct_split, value, totals, region_summary)

# Look at region as a whole

region_summary2 <- value %>%
  group_by(year, variable) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

value_summary2 <- region_summary2 %>%
  mutate(variable = factor(variable, levels=value_order_old))


value_chart_2021_reg <- static_column_chart(t = value_summary2 %>% filter(year==2021 & county=="Region Total"),
                                            x = "variable", y="estimate", fill="variable", color="obgnpgy_5",
                                            title="Home Values for Region: 2021",
                                            source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25075") +  
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


value_chart_2022_reg <- static_column_chart(t = value_summary2 %>% filter(year==2022 & county=="Region Total"),
                                            x = "variable", y="estimate", fill="variable", color="obgnpgy_5",
                                            title="Home Values for Region: 2022",
                                            source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25075") +  
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Bedroom Count --------------------------------------------------------------

# Download ACS data and clean
bedrooms <- get_acs_recs(geography = 'block group', table.names = c('B25042'), years = years)

bedrooms <- bedrooms %>%
  select(geoid="GEOID", geography="census_geography", "estimate", "moe", "variable", "year", "label") %>%
  filter(grepl('Owner', label)) %>%
  separate(variable, into=c("table","variable"), sep="_", remove=TRUE) %>%
  mutate(variable = as.integer(variable)) %>%
  mutate(label = case_when(variable ==002 ~ "Total",
                           variable ==003 ~ "No bedroom",
                           variable ==004 ~ "1 bedroom",
                           variable ==005 ~ "2 bedrooms",
                           variable ==006 ~ "3 bedrooms",
                           variable ==007 ~ "4 bedrooms",
                           variable ==008 ~ "5+ bedrooms")) %>%
  select(-"geography",-"table",-"variable") %>%
  group_by(geoid, year, label) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  rename(variable="label") %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))

# Download 2010 ACS data and clean
bedrooms10 <- read_csv("J:\\Projects\\V2050\\Housing\\Monitoring\\2024Update\\Data\\metric10_ownership_near_transit\\2010_files\\nhgis0001_csv\\B25042\\bedrooms-2010-5yr.csv") %>% 
  rename(geoid="GEOID") %>%
  rename(year="YEAR") %>%
  mutate(geoid = str_remove_all(geoid, "15000US")) %>% 
  select("year", "geoid", "estimate-002", "estimate-003", "estimate-004", "estimate-005", "estimate-006", "estimate-007", "estimate-008", "moe-002", "moe-003", "moe-004", "moe-005", "moe-006", "moe-007", "moe-008") %>%
  mutate(county=case_when(
  str_detect(geoid, "53033") ~ "King County",
  str_detect(geoid, "53035") ~ "Kitsap County",
  str_detect(geoid, "53053") ~ "Pierce County",
  str_detect(geoid, "53061") ~ "Snohomish County"))

bedrooms10_pivoted <- bedrooms10 %>% 
  pivot_longer(cols = !c(year, county, geoid), names_to = c(".value", "variable"), names_sep = "-") %>%
  mutate(variable = factor(variable, levels=c("002", "003", "004", "005", "006", "007", "008"))) %>%
  mutate(variable = case_when(variable =="002" ~ "Total",
                           variable =="003" ~ "No bedroom",
                           variable =="004" ~ "1 bedroom",
                           variable =="005" ~ "2 bedrooms",
                           variable =="006" ~ "3 bedrooms",
                           variable =="007" ~ "4 bedrooms",
                           variable =="008" ~ "5+ bedrooms")) %>%
  as_tibble() %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))


#Combine 2010 with Other Years

bedrooms_all <- bind_rows(bedrooms, bedrooms10_pivoted)

# Calculate Estimates using Blockgroup splits from Elmer Parcelization Data 
in_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="in station area or rgc") %>%
  select(geoid="data_geog", year="ofm_estimate_year", inside_split="percent_of_occupied_housing_units")

out_hct_split <- hct_blockgroup %>%
  filter(planning_geog=="not in station area or rgc") %>%
  select(geoid="data_geog", year="ofm_estimate_year", outside_split="percent_of_occupied_housing_units")

bedrooms_by_hct <- left_join(bedrooms_all, in_hct_split, by=c("geoid", "year")) %>% 
  mutate(inside_split=replace_na(inside_split, 0)) %>%
  mutate(inside_estimate = round(estimate*inside_split,0))

bedrooms_by_hct <- left_join(bedrooms_by_hct, out_hct_split, by=c("geoid", "year")) %>% 
  mutate(outside_split=replace_na(outside_split, 0)) %>%
  mutate(outside_estimate = round(estimate*outside_split,0))

# Create Clean Blockgroup Data
bedrooms_by_hct <- bedrooms_by_hct %>%
  select("geoid","county","variable","year",`Inside centers and HCT station areas`="inside_estimate",`Outside centers and HCT station areas`="outside_estimate") %>%
  pivot_longer(!c(geoid,county,variable,year), names_to = "metric", values_to = "estimate") 

# Create Summary Chart
bedrooms_summary <- bedrooms_by_hct %>%
  group_by(county, year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

region_summary <- bedrooms_by_hct %>%
  group_by(year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

bedrooms_summary <- bind_rows(bedrooms_summary, region_summary) %>%
  mutate(county = factor(county, levels=county_order)) %>%
  mutate(variable = factor(variable, levels=bedrooms_order))

totals <- bedrooms_summary %>% filter(variable=="Total") %>% rename(total=estimate) %>% select(-"variable")

bedrooms_summary <- left_join(bedrooms_summary, totals, by=c("county","year", "metric")) %>%
  mutate(share=estimate/total) %>%
  select(-"total")

bedrooms_chart_2010 <- static_column_chart(t = bedrooms_summary %>% filter(year==2010 & variable!="Total" & county=="Region Total"),
                                           x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                           title="Ownership Housing by Number of Bedrooms: 2010",
                                           source="Source: US Census Bureau 2006-2010 American Community Survey 5-year Estimates Table B25042") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

bedrooms_chart_2017 <- static_column_chart(t = bedrooms_summary %>% filter(year==2017 & variable!="Total" & county=="Region Total"),
                                           x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                           title="Ownership Housing by Number of Bedrooms: 2017",
                                           source="Source: US Census Bureau 2013-2017 American Community Survey 5-year Estimates Table B25042") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

bedrooms_chart_2021 <- static_column_chart(t = bedrooms_summary %>% filter(year==2021 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Ownership Housing by Number of Bedrooms: 2021",
                                        source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25042") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

bedrooms_chart_2022 <- static_column_chart(t = bedrooms_summary %>% filter(year==2022 & variable!="Total" & county=="Region Total"),
                                        x = "variable", y="share", fill="metric", color="obgnpgy_5",
                                        title="Ownership Housing by Number of Bedrooms: 2022",
                                        source="Source: US Census Bureau 2018-2022 American Community Survey 5-year Estimates Table B25042") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

rm(in_hct_split, out_hct_split, totals, region_summary)

write_csv(tenure_summary, file = 'J:/Projects/V2050/Housing/Monitoring/2024Update/Data/metric10_ownership_near_transit\\ownershipbedrooms2024.csv')
