# Collection of scripts for PUMS processing

## Renter Cost Burden by Race

For a given set of years and using the `psrccensus` package, the `renter_cost_burden_by_race.R` script loads PUMS data on renters (TEN), income (HINCP), race (PRACE) and rent as a percentage of income (GRIP). It groups the GRIP variable into four categories (< 30%, 30-50%, > 50%, NA). Two interactive time series plots are generated for each of the GRIP groups "> 50%" (or "severe rent burden") and "30-50%", one segmented by race groups and one segmented by income groups. The underlying data is optionally stored into an Excel spreadsheet.

To use this script, one will need to obtain a [FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html), see `?fredr-key` for setting it up.

