tidy_ofm_apr <- function(dataset, tabname) {

  # This function will clean and transform a single sheet either post-censal (population or housing) or intercensal April 1 data from OFM. 
  # Intercensal data has a different format than the postcensal (e.g. additional Place Code columns, column names, Jurisdiction string format) 

  d <- setDT(dataset)
  
  if('County.Name' %in% colnames(d) && 'City.Name' %in% colnames(d)) {
    dtype <- 'intercensal'
  } else if(length(str_subset(colnames(d), 'Two.or.More.Unit.Housing.Units')) > 1) {
    dtype <- 'housing'
  } else {
    dtype <- 'post'
  } 

  tabname <-  str_to_lower(tabname)
  tabname <-  str_replace_all(tabname, " ", '_')
  
  if(tabname == 'population') 'total_population'
  if(tabname == 'gq_population') 'group_quarters_population'
  if(tabname == 'occupied_housing') 'occupied_housing_units'
  if(tabname == 'total_housing') 'housing_units'

  # intercensal specific munging
  if('County.Name' %in% colnames(d) && 'City.Name' %in% colnames(d)) {
    setnames(d, 'County.Name', 'County')
    d <- d[, Jurisdiction := str_replace_all(Jurisdiction, 'city', '')]
    d <- d[, Jurisdiction := str_replace_all(Jurisdiction, 'town', '')]
    d <- d[, Jurisdiction := str_replace_all(Jurisdiction, '\\s+', ' ')]
    d <- d[, Jurisdiction := str_replace_all(Jurisdiction, '\\s$', '')]
    rm.cols <- c('City.Name', str_subset(colnames(d), '.*Code$'))
    d <- d[, !..rm.cols]
  }

  # filter for region
  counties <- c('King', 'Kitsap', 'Pierce', 'Snohomish')
  reg <- d[County %in% counties, ]
  
  # remove Line column, filter out 1 & 3
  reg <- reg[!(Filter %in% c(1, 3)), ][, !c('Line', 'Filter')]

  # pivot longer
  reg.pvt <- melt(reg, id.vars = c('County', 'Jurisdiction'), variable.name = 'desc', value.name = tabname)
  reg.pvt[, estimate_year := str_extract(desc, '^\\d+')]

  if(dtype == 'housing') {
    reg.pvt[, housing_type := str_extract(desc, '((?<=of\\.).*)')]
    reg.pvt[, housing_type := str_replace_all(housing_type, '\\.', '_')]
    reg.pvt[, housing_type := str_replace_all(housing_type, '\\W$', '')]
    reg.pvt[, housing_type := str_replace_all(housing_type, 'Moble', 'Mobile')]
    reg.pvt[, housing_type := str_to_lower(housing_type)]
  } 
  
  # munge
  reg.pvt[, desc := NULL]
  colnames(reg.pvt) <- str_to_lower(colnames(reg.pvt))
  
  if(dtype != 'housing') {
    # reorder
    cols <- c("county", "jurisdiction", "estimate_year", tabname)
    setcolorder(reg.pvt, cols)
    
  }

  # convert to numeric cols
  reg.pvt[, (tabname) := as.numeric(get(eval(tabname)))]
  
  if(dtype == 'housing') {
    reg.pvt <- dcast(reg.pvt, county + jurisdiction + estimate_year ~ housing_type, value.var = 'housing_units')
    cols <-  c('county', 'jurisdiction', 'estimate_year', 'total_housing_units', 'one_unit_housing_units', 'two_or_more_unit_housing_units', 'mobile_homes_and_specials')
    setcolorder(reg.pvt, cols)
  }
  
  return(reg.pvt)
}
