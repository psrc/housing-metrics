library(psrccensus)
library(magrittr)
library(dplyr)

years   <- c(2014, 2019, 2024)
dir = "C:/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

# 1. Setup: List necessary direct-PUMS variables & table names ------
# PUMS variables for population-scale analysis

hvars <- c(
  "AGEP",                   # Householder age
  "HINCP",                  # Household income
  "PRACE",                  # Respondent race (PSRC categories)
  "HRACE",                  # Household race (PSRC categories)
  "NP",                     # Number of persons in household
  "OWN_RENT",               # Simplified tenure
  "GRPIP",                  # Gross rent as percentage of income
  "GRNTP")                  # Gross rent

# 2. Setup: Helper functions ----------------------------------------
# Age categories
bin_age <- function(so){
  so %<>% mutate(
    age_bin5 = factor(case_when(
      AGEP < 18 ~ "under 18 years",
      AGEP < 36 ~ "between 18 and 35 years",
      AGEP < 65 ~ "between 36 and 64 years",
      AGEP < 85 ~ "between 65 and 84 years",
      AGEP > 84 ~ "85 years and over",
      !is.na(AGEP) ~ "Else / Prefer not to answer"),
      levels=c("under 18 years",
              "between 18 and 35 years",
              "between 36 and 64 years",
              "between 65 and 84 years",
              "Else / Prefer not to answer")))
  return(so)
}

# Define rent burden and HUD-inspired percent AMI variables
add_hh_vars <- function(so, ami) {
  # srvyr has no join method, so join the AMI lookup onto the underlying data
  ami_lookup <- ami %>%
    transmute(msa = as.character(msa),
              hud_sz = as.character(hud_sz),
              HINCP_median)
  so$variables <- so$variables %>%
    mutate(msa = as.character(msa), hud_sz = as.character(hud_sz)) %>%
    left_join(ami_lookup, by = c("msa", "hud_sz"))

  ami_ratio <- so$variables$HINCP / so$variables$HINCP_median

  so %<>% mutate(
    rent_burden = factor(case_when(
      GRPIP < 30 | (is.na(GRNTP) & OWN_RENT == "Rented") ~ "Under 30 percent",
      dplyr::between(GRPIP, 30, 50) ~ "30-50 percent",
      GRPIP > 50 | is.na(HINCP) ~ "Over 50 percent"),
    levels = c("Over 50 percent",
                 "30-50 percent",
                 "Under 30 percent")),
    share_ami = factor(case_when(
      ami_ratio >= 1.2 ~ "120+",
      ami_ratio >= 1   ~ "100-119",
      ami_ratio >= 0.8 ~ "80-99",
      ami_ratio >= 0.5 ~ "50-79",
      ami_ratio >= 0.3 ~ "30-49",
      ami_ratio < 0.3  ~ "< 30"
    ),
    levels = c("< 30", "30-49", "50-79", "80-99", "100-119", "120+"))
  )
}

# Function for county + region counts
ctyreg_pums_count <- function(so, group_vars=NULL) 
  rs      <- list()
  rs[[1]] <- psrc_pums_count(so, group_vars=group_vars, incl_na=FALSE) 
  rs[[2]] <- psrc_pums_count(so, group_vars=c("COUNTY", group_vars), incl_na=FALSE) %>%
    filter(COUNTY!="Region")
  rs %<>% data.table::rbindlist() %>% arrange(DATA_YEAR, COUNTY)
  return(rs)
}

# Function for county + region median
ctyreg_pums_median <- function(so, stat_var, group_vars=NULL){
  rs      <- list()
  rs[[1]] <- psrc_pums_median(so, stat_var, group_vars=group_vars, incl_na=FALSE)  
  rs[[2]] <- psrc_pums_median(so, stat_var, group_vars=c("COUNTY", group_vars), incl_na=FALSE) %>%
    filter(COUNTY!="Region")
  rs %<>% data.table::rbindlist() %>% arrange(DATA_YEAR, COUNTY)
  return(rs)
}

# Function for county + region mean
ctyreg_pums_mean <- function(so, stat_var, group_vars=NULL){
  rs      <- list()
  rs[[1]] <- psrc_pums_mean(so, stat_var, group_vars=group_vars, incl_na=FALSE)  
  rs[[2]] <- psrc_pums_mean(so, stat_var, group_vars=c("COUNTY", group_vars), incl_na=FALSE) %>%
    filter(COUNTY!="Region")
  rs %<>% data.table::rbindlist() %>% arrange(DATA_YEAR, COUNTY)
  return(rs)
}

write_rhna_pums <- function(result_list){
  openxlsx::write.xlsx(result_list, file = "rhna_pums.xlsx", rowNames = FALSE)
  return(invisible(NULL))
}

# 3. Main -----------------------------------------------------------
get_singleyear_results <- function(survey_year){
  pp_so <- get_psrc_pums(5, dyear = survey_year, "p", "AGEP", dir = dir)
  hh_so <- get_psrc_pums(5, dyear = survey_year, "h", hvars, dir = dir) %>% bin_age()

  hh_so %<>% mutate(
    msa = factor(if_else(COUNTY %in% c("King","Snohomish"), "Seattle-Bellevue", COUNTY)),
    hud_sz = factor(if_else(NP > 8, 8, NP))
  )
  
  hud_ami <- psrc_pums_median(hh_so, stat_var = "HINCP", group_vars = c("msa", "hud_sz"))
  
  hh_so %<>% add_hh_vars(hud_ami)
  
  results <- list()
  results$`1d-Population by Race P`  <- ctyreg_pums_count(pp_so, group_vars = "PRACE")
  results$`1d-Population by Race H`  <- ctyreg_pums_count(pp_so, group_vars = "HRACE")
  results$`3a-Avg HH size`           <- ctyreg_pums_mean(hh_so, stat_var = "NP")
  results$`3c-Avg HH size by Age`    <- ctyreg_pums_mean(hh_so, stat_var = "NP", group_vars = "age_bin5")
  results$`3d-Avg HH size by Race P` <- ctyreg_pums_mean(hh_so, stat_var = "NP", group_vars = "PRACE")
  results$`3d-Avg HH size by Race H` <- ctyreg_pums_mean(hh_so, stat_var = "NP", group_vars = "HRACE")
  results$`3e-Avg HH size by %AMI`   <- ctyreg_pums_mean(hh_so, stat_var = "NP", group_vars = "share_ami")
  results$`4d-Tenure by Race P`      <- ctyreg_pums_count(hh_so, group_vars = c("OWN_RENT", "PRACE"))
  results$`4d-Tenure by Race H`      <- ctyreg_pums_count(hh_so, group_vars = c("OWN_RENT", "HRACE"))
  results$`4e-Tenure by %AMI`        <- ctyreg_pums_count(hh_so, group_vars = c("OWN_RENT", "share_ami"))
  results$`5a-Income`                <- ctyreg_pums_median(hh_so, stat_var = "HINCP")
  results$`5c-Income by Age`         <- ctyreg_pums_median(hh_so, stat_var = "HINCP", group_vars = "age_bin5")
  results$`5e-Income by Race P`      <- ctyreg_pums_median(hh_so, stat_var = "HINCP", group_vars = "PRACE")
  results$`5e-Income by Race H`      <- ctyreg_pums_median(hh_so, stat_var = "HINCP", group_vars = "HRACE")
  results$`12ab-Rent burden`         <- ctyreg_pums_count(hh_so, group_vars = "rent_burden")
  results$`12c-Rent burden by Age`   <- ctyreg_pums_count(hh_so, group_vars = c("age_bin5", "rent_burden"))
  results$`12d-Rent burden by Race P`<- ctyreg_pums_count(hh_so, group_vars = c("PRACE", "rent_burden"))
  results$`12d-Rent burden by Race H`<- ctyreg_pums_count(hh_so, group_vars = c("HRACE", "rent_burden"))
  results$`12e-Rent burden by %AMI`  <- ctyreg_pums_count(hh_so, group_vars = c("share_ami", "rent_burden"))

  return(results)
}

biglist <- lapply(years, get_singleyear_results) %>% setNames(years)

# One table per sheet, years stacked
combined <- purrr::list_transpose(biglist) %>% 
  lapply(data.table::rbindlist, fill = TRUE)

write_rhna_pums(combined)
