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
psrc_region_3cnty <- c("Everett/Snohomish County CoC", "Tacoma/Lakewood/Pierce County CoC", "Seattle/King County CoC")
data_years <- (2014:2024)
most_recent_year <- "2024"

# Functions ---------------------
coc_major_city_lookup <- openxlsx::read.xlsx(pit_path, sheet = most_recent_year, skipEmptyRows = TRUE, colNames = TRUE) |> as_tibble() |> filter(CoC.Category == "Major City CoC") |> distinct(CoC.Number)

coc_region_lookup <- openxlsx::read.xlsx(pit_path, sheet = most_recent_year, skipEmptyRows = TRUE, colNames = TRUE) |> as_tibble() |> filter(CoC.Name %in% psrc_region_3cnty) |> distinct(CoC.Number)

coc_lookup <- bind_rows(coc_major_city_lookup, coc_region_lookup) |> distinct(CoC.Number)
rm(coc_major_city_lookup, coc_region_lookup)

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
    
  mutate(homeless_share_hisp = total_hisp/total,
         homeless_share_aian = total_aian/total,
         homeless_share_asian = total_asian/total,
         homeless_share_black = total_black/total,
         homeless_share_nhpi = total_nhpi/total,
         homeless_share_white = total_white/total,
         homeless_share_multiracial = total_multiracial/total,
    
         unsheltered_share_total = unsheltered/total,
         unsheltered_share_hisp = unsheltered_hisp/unsheltered,
         unsheltered_share_aian = unsheltered_aian/unsheltered,
         unsheltered_share_asian = unsheltered_asian/unsheltered,
         unsheltered_share_black = unsheltered_black/unsheltered,
         unsheltered_share_nhpi = unsheltered_nhpi/unsheltered,
         unsheltered_share_white = unsheltered_white/unsheltered,
         unsheltered_share_multiracial = unsheltered_multiracial/unsheltered) |>
  filter(count_type == "Sheltered and Unsheltered Count")
})

# Major city comparison, snapshot
major_cities_snapshot <- hud_data |>
  filter(year == most_recent_year) |> 
  arrange(desc(unsheltered_share_total), .by_group = FALSE)

# PSRC region over time
psrc_region <- hud_data |>
  filter(coc_name %in% psrc_region_3cnty) |> 
  arrange(coc_name, .by_group = FALSE)

write.xlsx(list("major cities snapshot" = major_cities_snapshot, "PSRC 3 cnty" = psrc_region), file = file.path(file_loc, file = "coc_exploration_export.xlsx"))
