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

earliest_year <- "2015"
most_recent_year <- "2024"
data_years <- (earliest_year:most_recent_year)

# Functions ---------------------
coc_major_city_lookup <- openxlsx::read.xlsx(pit_path, sheet = most_recent_year, skipEmptyRows = TRUE, colNames = TRUE) |> as_tibble() |> filter(CoC.Category == "Major City CoC") |> distinct(CoC.Number)

coc_region_lookup <- openxlsx::read.xlsx(pit_path, sheet = most_recent_year, skipEmptyRows = TRUE, colNames = TRUE) |> as_tibble() |> filter(CoC.Name %in% psrc_region_3cnty) |> distinct(CoC.Number)

coc_lookup <- bind_rows(coc_major_city_lookup, coc_region_lookup) |> distinct(CoC.Number)
rm(coc_major_city_lookup, coc_region_lookup)

load_pit_data <- function(data_years, pit_path, coc_lookup) {
  
  map_dfr(data_years, function(yr) {
    
    print(glue::glue("Processing PIT: {yr}"))
    
    df <- dplyr::as_tibble(
      openxlsx::read.xlsx(
        pit_path,
        sheet = as.character(yr),
        detectDates = FALSE,
        skipEmptyRows = TRUE,
        colNames = TRUE
      )
    ) |>
      mutate(year = yr) |>
      dplyr::filter(CoC.Number %in% dplyr::pull(coc_lookup, CoC.Number))
    
    df <- df |>
      mutate(across(
        c(
          Overall.Homeless,
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
          `Unsheltered.Homeless.-.Multi-Racial`
        ),
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
        unsheltered_multiracial = `Unsheltered.Homeless.-.Multi-Racial`
      ) |>
      filter(count_type == "Sheltered and Unsheltered Count")
    
  })
}

crunch_data_cleanup <- function(df) {
  df <- df |> #Change order of final output
    mutate(homeless_share_hisp = total_hisp/total,
           homeless_share_aian = total_aian/total,
           homeless_share_asian = total_asian/total,
           homeless_share_black = total_black/total,
           homeless_share_nhpi = total_nhpi/total,
           homeless_share_white = total_white/total,
           homeless_share_multiracial = total_multiracial/total,
           
           unsheltered_rate_total = unsheltered/total,
           unsheltered_share_hisp = unsheltered_hisp/unsheltered,
           unsheltered_share_aian = unsheltered_aian/unsheltered,
           unsheltered_share_asian = unsheltered_asian/unsheltered,
           unsheltered_share_black = unsheltered_black/unsheltered,
           unsheltered_share_nhpi = unsheltered_nhpi/unsheltered,
           unsheltered_share_white = unsheltered_white/unsheltered,
           unsheltered_share_multiracial = unsheltered_multiracial/unsheltered) |>
    select(year,
           coc_num,
           coc_name,
           
           total,
           sheltered,
           unsheltered,
           unsheltered_rate_total,
           
           total_hisp,
           total_aian,
           total_asian,
           total_black,
           total_nhpi,
           total_white,
           total_multiracial,
           
           unsheltered_hisp,
           unsheltered_aian,
           unsheltered_asian,
           unsheltered_black,
           unsheltered_nhpi,
           unsheltered_white,
           unsheltered_multiracial,
           
           homeless_share_hisp,
           unsheltered_share_hisp,
           homeless_share_aian,
           unsheltered_share_aian,
           homeless_share_asian,
           unsheltered_share_asian,
           homeless_share_black,
           unsheltered_share_black,
           homeless_share_nhpi,
           unsheltered_share_nhpi,
           homeless_share_white,
           unsheltered_share_white,
           homeless_share_multiracial,
           unsheltered_share_multiracial)}

change_over_time <- function(df) {
  df <- df |> # calculate change over time
    filter(year >= earliest_year & year <= most_recent_year) |>
    group_by(coc_name) |>
    summarise(
      start_year = min(year),
      end_year = max(year),
      total_pct_change = (last(total[order(year)]) - first(total[order(year)])) / first(total[order(year)]),
      sheltered_pct_change = (last(sheltered[order(year)]) - first(sheltered[order(year)])) / first(sheltered[order(year)]),
      unsheltered_pct_change = (last(unsheltered[order(year)]) - first(unsheltered[order(year)])) / first(unsheltered[order(year)]),
      .groups = "drop")
    }


# Analyses ---------------------

# Run functions to create HUD dataset
hud_data <- load_pit_data(data_years, pit_path, coc_lookup)
hud_data <- crunch_data_cleanup(hud_data)

# Major cities comparison ---
major_cities_change <- change_over_time(hud_data)

major_cities_snapshot <- hud_data |>
  group_by(coc_name) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup()

major_cities_final <- left_join(major_cities_change, major_cities_snapshot, by = "coc_name") |>
  select(coc_num, coc_name, start_year, end_year, total, sheltered, unsheltered, total_pct_change, sheltered_pct_change, unsheltered_pct_change, unsheltered_rate_total) |>
  arrange(desc(unsheltered_rate_total))

rm(major_cities_change, major_cities_snapshot)

# PSRC region over time ---
psrc_3cnty <- load_pit_data(data_years, pit_path, coc_lookup) |>
  filter(coc_name %in% psrc_region_3cnty) |> 
  arrange(coc_name, .by_group = FALSE)

psrc_region <- psrc_3cnty |>
  group_by(year) |>
  filter(n_distinct(coc_num) == 3) |>
  summarise(across(starts_with(c("total", "sheltered", "unsheltered")), sum, na.rm = TRUE),
            coc_name = "PSRC Region – King, Pierce, Snohomish")

psrc_region <- dplyr::bind_rows(psrc_region, psrc_3cnty)
psrc_region <- crunch_data_cleanup(psrc_region)

psrc_region_change <- change_over_time(psrc_region)

psrc_region_snapshot <- psrc_region |>
  group_by(coc_name) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup()

psrc_region_final <- left_join(psrc_region_change, psrc_region_snapshot, by = "coc_name") |>
  select(coc_num, coc_name, start_year, end_year, total, sheltered, unsheltered, total_pct_change, sheltered_pct_change, unsheltered_pct_change, unsheltered_rate_total) |>
  arrange(desc(unsheltered_rate_total))

rm(psrc_3cnty, psrc_region_change, psrc_region_snapshot)

write.xlsx(list("major cities comparison" = major_cities_final, "PSRC 3 cnty" = psrc_region_final), file = file.path(file_loc, file = "coc_exploration_export.xlsx"))
