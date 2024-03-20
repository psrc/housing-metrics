# See API documentation: https://www.huduser.gov/portal/dataset/chas-api.html
# This script accesses the CHAS API to query county level summary data
# requires a personal access token stored in a text file. Modify file path on line #27.

# To specify a specific year range other than the most current, add year argument (e.g. year=2014-2018) in line 38 of build_url_query_param()
# See documentation for all available year ranges.

library(here)
library(httr)
library(data.table)
library(purrr)

build_url_query_param <- function(...) {
  # Build the API endpoint with query parameters 
  kwargs <- list(...)
  params <- c()
  if('type' %in% names(kwargs)) params <- append(params, paste0('type=', kwargs$type))
  if('year' %in% names(kwargs)) params <- append(params, paste0('year=', kwargs$year)) 
  if('state' %in% names(kwargs)) params <- append(params, paste0('stateId=', kwargs$state))
  if('county' %in% names(kwargs)) params <- append(params, paste0('entityId=', kwargs$county))
  url <- "https://www.huduser.gov/hudapi/public/chas"
  query <- paste0('?', paste(params, collapse = '&'))
  endpoint <- paste0(url, query)
  return(endpoint)
}

# store pat in txt and read-in
pat_file <- read.csv('H:/chas-pat.txt', header = FALSE)
pat <- pat_file[[1]][1]

# read-in file of data-dictionary
prop_dict <- read.csv(here('process', 'CHAS', 'chas-response-prop.csv'))

# request county summaries and convert to dataframe
counties <- c(33, 35, 53, 61)

dt_all <- NULL
for(county in counties) {
  url <- build_url_query_param(type = 3, state = 53, county = county)
  response <- GET(url, add_headers(Authorization = paste("Bearer ", pat)))
  cont <- content(response)
  c <- flatten(cont)
  dt <- rbindlist(list(c))
  ifelse(is.null(dt_all), dt_all <- dt, dt_all <- rbindlist(list(dt_all, dt)))
}

# wrangle results
d <- melt(dt_all, measure.vars = patterns('\\w\\d'), variable.name = 'property', value.name = 'households')
d_join <- merge(d, prop_dict, by = 'property')
df <- setcolorder(d_join, c('geoname', 'sumlevel', 'year', 'property', 'description', 'households'))

