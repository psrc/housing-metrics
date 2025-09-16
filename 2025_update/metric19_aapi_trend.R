library(psrccensus)
library(psrcplot)
library(tidyverse)

# install psrccensus and get api key by going trough instructions on: https://psrc.github.io/psrccensus/articles/psrccensus.html
Sys.getenv("CENSUS_API_KEY")

# more information on PUMS data: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# 2023 5-year PUMS data dictionary: https://api.census.gov/data/2023/acs/acs5/pums/variables.html

# list of data
# 
# ___FULL DATASETS___
# 1. df_pums: all households 
# 2. df_pums_p: all persons
# 
# ___AAPI HOUSEHOLD DATASETS___
# 3.df_pums_aapi: all AAPI households                                                      (use: total household count/ tenure/ income)
#   - new variables: RAC2P_equivalent_aapi_group10 
#     (grouped race category: top 10 populous Asian subgroups, other Asian 
#      subgroups and NH&PI)
# 4. df_pums_renter_aapi: all AAPI renter households                                       (use: cost burden)
# 5. df_pums_aapi_allpersons: all households with any AAPI member
# 
# ___AAPI PERSONS DATASETS___
# 6. df_pums_p_aapi_renter_worker: (person-level) all adults in AAPI renters households    (use: occupation)
#    - new variables: RAC2P_equivalent_aapi_group10_household (workers in RAC2P_equivalent_aapi_group10 households)

# ---- full datasets ---- 

# download 2023 5-year PUMS data with specified variables
pums_2023_h <- get_psrc_pums(span = 5,
                             dyear = 2023,
                             level = "h",
                             vars = c("AGEP",  # Age
                                      "PRACE", # Race
                                      "RAC1P", # Recoded detailed race code
                                      "RAC2P19", # Recoded detailed race code (2019 and prior)
                                      "RAC2P23", # Recoded detaile drace code (2023 and later)
                                      "TEN",   # Tenure
                                      "GRPIP", # Gross rent as a percentage of household income past 12 months
                                      "HINCP",  # Household income
                                      "BIN_POVRATIO",
                                      "SOCP3")) 
## ----- 1. households ----- 
df_pums <- pums_2023_h %>%
  # make new variables
  mutate(tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", 
                                 TRUE ~"renter"),
                       levels=c("owner", "renter")),
         race_aapi = case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                               PRACE == "White alone" ~ "White alone",
                               TRUE ~ PRACE),
         rent_pct_income = factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                            between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                                            GRPIP > 50 ~ "Greater than 50 percent",
                                            TRUE ~ "No rent paid"),
                                  levels=c("Greater than 50 percent",
                                           "Between 30 and 50 percent",
                                           "Less than 30 percent",
                                           "No rent paid")),
         rent_pct_income_30 = factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                            GRPIP >= 30 ~ "Greater than 30 percent",
                                            TRUE ~ "No rent paid"),
                                  levels=c("Greater than 30 percent",
                                           "Less than 30 percent",
                                           "No rent paid")),
         income_poverty_level = case_when(BIN_POVRATIO %in% c("under 0.50","0.50 to 0.99")~"Income below 100% of poverty level",
                                          TRUE~"Income above 100% of poverty level"),
         PRACE = case_when(PRACE=="Hispanic or Latino"~"Hispanic/Latinx",
                           PRACE=="American Indian or Alaskan Native Alone"~"American Indian or Alaskan Native alone",
                           PRACE=="Some Other Race alone"~"Another Racial Identity",
                           PRACE=="Two or More Races"~"Multiracial", 
                           TRUE~PRACE),
         RAC2P_equivalent = coalesce(
           na_if(RAC2P19, "Code classification is Not Applicable because data are 2023 vintage"),
           na_if(RAC2P23, "Code classification is Not Applicable because data are 2019-2022 vintage")),
         RAC2P_equivalent = recode(RAC2P_equivalent,
           "Chinese, except Taiwanese alone" = "Chinese, except Taiwanese, alone"))

## ----- 2. persons ----- 
pums_2023_p <- get_psrc_pums(span = 5,
                             dyear = 2023,
                             level = "p",
                             vars = c("AGEP",
                                      "TYPEHUGQ",
                                      "PRACE",
                                      "RAC1P",
                                      "RAC2P19",
                                      "RAC2P23",
                                      "SOCP3",
                                      "SOCP5",
                                      "TEN",
                                      "WAGP"
                             )) 
df_pums_p <- pums_2023_p %>%
  mutate(PRACE = case_when(PRACE=="Hispanic or Latino"~"Hispanic/Latinx",
                           PRACE=="American Indian or Alaskan Native Alone"~"American Indian or Alaskan Native alone",
                           PRACE=="Some Other Race alone"~"Another Racial Identity",
                           PRACE=="Two or More Races"~"Multiracial", 
                           TRUE~PRACE),
         RAC2P_equivalent = coalesce(
           na_if(RAC2P19, "Code classification is Not Applicable because data are 2023 vintage"),
           na_if(RAC2P23, "Code classification is Not Applicable because data are 2019-2022 vintage")),
         RAC2P_equivalent = recode(RAC2P_equivalent,
                                   "Chinese, except Taiwanese alone" = "Chinese, except Taiwanese, alone"))

# ---- AAPI households data ----

# top 10 Asian subgroups with the most households
asian_top10 <- df_pums %>%
  filter(PRACE == "Asian alone") %>%
  psrc_pums_count(., group_vars=c("PRACE","RAC2P_equivalent")) %>%
  filter(!RAC2P_equivalent %in% c("All combinations of Asian races only","Other Asian alone","Total")) %>%
  arrange(desc(count)) %>%
  top_n(7,count)


## ----- 3. AAPI households (householder) ----- 
df_pums_aapi <- df_pums %>%
  filter(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) %>%
  mutate(
    # grouped race category: top 10 populous Asian subgroups, other Asian races and all Pacific Islander
    RAC2P_equivalent_aapi_group10 = case_when(PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                                   PRACE == "Asian alone" & RAC2P_equivalent %in% asian_top10$RAC2P_equivalent~ RAC2P_equivalent,
                                   PRACE == "Asian alone"~ "Other Asian subgroups"),
    RAC2P_equivalent_income = case_when(PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                             RAC2P_equivalent %in% c("Asian Indian alone","Cambodian alone","Chinese, except Taiwanese, alone",
                                          "Filipino alone","Japanese alone","Korean alone","Laotian alone","Pakistani alone",
                                          "Taiwanese alone","Thai alone","Vietnamese alone")~RAC2P_equivalent,
                             RAC2P_equivalent == "All combinations of Asian races only"~"Two or more Asian",
                             TRUE~"Other Asian"))


## ----- 4. All renter households -----
df_pums_renter <- df_pums %>% filter(TEN=="Rented")
## ----- 4. AAPI renter households (householder) -----
df_pums_renter_aapi <- df_pums_aapi %>% filter(TEN=="Rented")

## ----- 5. AAPI households (any AAPI member in household) ----- 

# create "RAC2P_equivalent_allpersons" variable: get households with at least one AAPI member
race_allpersons <- df_pums_p[['variables']] %>% 
  # filter only AAPI adults
  filter(AGEP >= 15,
         TYPEHUGQ == "Housing unit",
         PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) %>%
  group_by(SERIALNO) %>%
  summarise(n_aapi = n(),
            n_prace = length(unique(PRACE)),
            n_RAC2P_equivalent = length(unique(RAC2P_equivalent)),
            all_prace = paste(unique(PRACE),collapse = "; "),
            all_RAC2P_equivalent = paste(unique(RAC2P_equivalent),collapse = "; ")) %>%
  ungroup() %>%
  # at least one of AAPI member in household
  mutate(PRACE_allpersons = case_when(# both Asian and PI race in household
    n_prace>1~"Multiple AAPI races",
    # at least one AAPI member in household
    all_prace=="Asian alone"~ "Asian",
    all_prace=="Native Hawaiian and Other Pacific Islander alone"~ "Native Hawaiian and Other Pacific Islander"),
    RAC2P_equivalent_allpersons = case_when(# both Asian and PI race in household
      n_prace>1~"Multiple AAPI races", 
      # multiple asian subgroups in household
      all_prace == "Asian alone" & n_RAC2P_equivalent>1~"Multiple Asian subgroups", 
      # multiple PI subgroups in household
      all_prace == "Native Hawaiian and Other Pacific Islander alone" & n_RAC2P_equivalent>1~"Multiple Native Hawaiian and Other Pacific Islander subgroups",
      # at least one AAPI member in household
      all_prace=="Asian alone"~ all_RAC2P_equivalent,
      all_prace=="Native Hawaiian and Other Pacific Islander alone"~ all_RAC2P_equivalent)) %>%
  select(SERIALNO,PRACE_allpersons,RAC2P_equivalent_allpersons)


df_pums_aapi_allpersons <- df_pums %>%
  filter(SERIALNO %in% race_allpersons$SERIALNO)
df_pums_aapi_allpersons[['variables']] <- df_pums_aapi_allpersons[['variables']] %>%
  left_join(race_allpersons, by="SERIALNO") %>%
  mutate(
    # grouped race category: top 10 populous Asian subgroups, other Asian races and all Pacific Islander
    RAC2P_equivalent_allpersons_aapi_group10 = case_when(PRACE_allpersons == "Asian" & RAC2P_equivalent_allpersons %in% asian_top10$RAC2P_equivalent~ RAC2P_equivalent_allpersons,
                                              PRACE_allpersons == "Asian"~ "Other Asian subgroups",
                                              TRUE~PRACE_allpersons
    ))



# ---- 6. AAPI persons data for occupation ----
# all adults in AAPI households
# possible filtering alternatives: only AAPI adults
df_pums_p_aapi_renter_worker <- df_pums_p %>% 
  filter(AGEP >= 15,
         # PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone"),
         !is.na(SOCP3),
         SERIALNO %in% df_pums_renter_aapi[['variables']]$SERIALNO)
df_pums_p_aapi_renter_worker[['variables']] <- df_pums_p_aapi_renter_worker[['variables']] %>% 
  left_join(df_pums_renter_aapi[['variables']] %>% select(SERIALNO,PRACE,RAC2P_equivalent,RAC2P_equivalent_aapi_group10) %>%
              rename(RAC2P_equivalent_aapi_group10_household = RAC2P_equivalent_aapi_group10), 
            by="SERIALNO", suffix=c("","_houshold"))

  

# --- example crosstabs ----
# total number of households in each subgroup
hh_count <- psrc_pums_count(df_pums_renter_aapi, group_vars=c("PRACE","RAC2P_equivalent")) %>%
  filter(RAC2P_equivalent!="Total")
# job share for renters in entire region and each AAPI subgroup
job3_region <- df_pums_p %>% filter(AGEP >= 15, !is.na(SOCP3), TEN=="Rented") %>%
  psrc_pums_count(., group_vars=c("SOCP3"))
job3_by_aapi_race <- psrc_pums_count(df_pums_p_aapi_renter_worker, group_vars=c("PRACE","RAC2P_equivalent_aapi_group10_household","SOCP3"))

# top 5 occupations of renters in entire region and each AAPI subgroup
job3_region_top_5 <- job3_region %>%
  filter(SOCP3 != "Total") %>%
  arrange(desc(share)) %>%
  top_n(5, share) %>%
  mutate(RAC2P_equivalent_aapi_group10_household = "Region", .after="COUNTY")
job3_by_aapi_race_top_5 <- job3_by_aapi_race %>%
  filter(SOCP3 != "Total") %>%
  group_by(RAC2P_equivalent_aapi_group10_household) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  top_n(5, share) %>%
  ungroup() %>%
  add_row(job3_region_top_5)

