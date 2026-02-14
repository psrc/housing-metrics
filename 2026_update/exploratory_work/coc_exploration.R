# Exploration of homelessness rates/unsheltered rates
# Geographies: Region compared to other metros
# Created By: Eric Clute

# Assumptions ---------------------
library(dplyr)
library(openxlsx)
library(stringr)
library(purrr)

user_dir <- Sys.getenv("USERPROFILE")
file_loc <- file.path(user_dir, "github/housing-metrics/2026_update/exploratory_work")
pit_path <- file.path(file_loc, "2007-2024-PIT-Counts-by-CoC.xlsx")
data_years <- (2014:2024)

# Functions ---------------------
coc_lookup <- openxlsx::read.xlsx(pit_path, sheet = "2024", skipEmptyRows = TRUE, colNames = TRUE) |>
  as_tibble() |>
  filter(CoC.Category == "Major City CoC") |>
  distinct(CoC.Number)

hud_data <- map_dfr(data_years, function(yr) {
  print(str_glue("Processing PIT: {yr}"))
  df<- dplyr::as_tibble(openxlsx::read.xlsx(pit_path, sheet = as.character(yr), detectDates = FALSE, skipEmptyRows = TRUE, colNames = TRUE)) |>
  as_tibble() |>
  mutate(year = yr) |>
    filter(CoC.Number %in% pull(coc_lookup, CoC.Number))
  
  df <- df |>
    mutate(across(
      c(Overall.Homeless,
        `Overall.Homeless.-.Hispanic/Latina/e/o`,
        `Overall.Homeless.-.American.Indian,.Alaska.Native,.or.Indigenous`,
        `Overall.Homeless.-.Asian.or.Asian.American`,
        `Overall.Homeless.-.Black,.African.American,.or.African`,
        `Overall.Homeless.-.Native.Hawaiian.or.Other.Pacific.Islander`,
        `Overall.Homeless.-.White`,
        `Overall.Homeless.-.Multi-Racial`,
        
        `Sheltered.Total.Homeless`,
        
        `Unsheltered.Homeless`,
        `Unsheltered.Homeless.-.Hispanic/Latina/e/o`,
        `Unsheltered.Homeless.-.American.Indian,.Alaska.Native,.or.Indigenous`,
        `Unsheltered.Homeless.-.Asian.or.Asian.American`,
        `Unsheltered.Homeless.-.Black,.African.American,.or.African`,
        `Unsheltered.Homeless.-.Native.Hawaiian.or.Other.Pacific.Islander`, 
        `Unsheltered.Homeless.-.White`, 
        `Unsheltered.Homeless.-.Multi-Racial`),
      as.numeric
    ))
  
  df |>
    select(
      year,
      coc_num = CoC.Number,
      coc_name = CoC.Name,
      count_type = Count.Types,
      
      total = Overall.Homeless,
      total_hisp = `Overall.Homeless.-.Hispanic/Latina/e/o`,
      total_aian = `Overall.Homeless.-.American.Indian,.Alaska.Native,.or.Indigenous`,
      total_asian = `Overall.Homeless.-.Asian.or.Asian.American`,
      total_black = `Overall.Homeless.-.Black,.African.American,.or.African`,
      total_nhpi = `Overall.Homeless.-.Native.Hawaiian.or.Other.Pacific.Islander`,
      total_white = `Overall.Homeless.-.White`,
      total_multiracial = `Overall.Homeless.-.Multi-Racial`,
      
      sheltered = `Sheltered.Total.Homeless`,
      unsheltered = `Unsheltered.Homeless`,
      
      unsheltered_hisp = `Unsheltered.Homeless.-.Hispanic/Latina/e/o`,
      unsheltered_aian = `Unsheltered.Homeless.-.American.Indian,.Alaska.Native,.or.Indigenous`,
      unsheltered_asian = `Unsheltered.Homeless.-.Asian.or.Asian.American`,
      unsheltered_black = `Unsheltered.Homeless.-.Black,.African.American,.or.African`,
      unsheltered_nhpi = `Unsheltered.Homeless.-.Native.Hawaiian.or.Other.Pacific.Islander`,
      unsheltered_white = `Unsheltered.Homeless.-.White`,
      unsheltered_multiracial = `Unsheltered.Homeless.-.Multi-Racial`) |>
  mutate(unsheltered_rate = unsheltered/total) |>
  filter(count_type == "Sheltered and Unsheltered Count")
})
