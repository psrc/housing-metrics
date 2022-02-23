library(here)
library(openxlsx)
library(data.table)
library(stringr)
library(odbc)
library(DBI)

## download file ----
data_file_name <- '2014thru2018-140-csv.zip'
base_url <- 'https://www.huduser.gov/portal/datasets/cp'
url_for_file <- file.path(base_url, data_file_name)

download.file(url_for_file, here('data', data_file_name))

## unzip file ----
file_name_local <- here('data', data_file_name)
output_path_zip <- here('data', '140')

unzip(file_name_local, exdir = here('data'))

# We use tract-level data for CHAS Table 9 to produce Cost Burden by Tenure (Owner, Renter) 
# and Race/Ethnicity, along with an in-house crosswalk table between census tracts and county subareas 
# “Cost burden” is defined as monthly housing costs as a percentage of gross income; there are 4 cost burden categories: 
# 1) less than or equal to 30%, 
# 2) greater than 30% but less than or equal to 50%, 
# 3) greater than 50%, and 
# 4) cost burden not computed (no or negative income) 
# Race/ethnicity categories include: 
# 1) White alone, non-Hispanic, 
# 2) Black or African-American alone, non-Hispanic, 
# 3) Asian alone, non-Hispanic, 
# 4) American Indian or Alaska Native alone, non-Hispanic, 
# 5) Pacific Islander alone, non-Hispanic, 
# 6) Hispanic, any race, and 
# 7) Other (including multiple races, non-Hispanic).

# The first step involves pulling the relevant data fields from CHAS Table 9 for each census tract in the region.


# read in Table 9 Cost Burden by Tenure and Race & data dictionary ----


## extracted file names for table and dictionary ----
data_dict_name <- 'CHAS data dictionary 14-18.xlsx'
table_9_file_name <- 'Table9.csv'
elmer_table_name <- 'chas_tbl_9_2018'

table_9_path_name <- file.path(output_path_zip, table_9_file_name)
data_dict_path_name <- file.path(output_path_zip, data_dict_name)

dict_df <- read.xlsx(data_dict_path_name, sheet = 'Table 9')
setDT(dict_df)
dict_df[, `:=` (Column.Name = str_replace_all(Column.Name, 'est', ''),
                index = 1:nrow(dict_df))]
setnames(dict_df, colnames(dict_df), str_replace_all(colnames(dict_df), '\\.', ' '))

## munge Table 9 ----
tbl9 <- read.csv(table_9_path_name)
setDT(tbl9)

# filter data to PSRC tracts
counties <- c(33, 35, 53, 61)
reg_tbl9 <- tbl9[cnty %in% counties & st == 53, ]

# truncate geo_id to remove the part before US so it can join on our usual tract data
reg_tbl9[, `:=` (GEOID_short = str_extract(geoid, '(?<=S).*'),
                 id = 1:nrow(reg_tbl9))]

# pivot table to be long for each estimate, moe
reg_tbl9_l <- melt(reg_tbl9, 
                   variable.name = 'measurement_id',
                   measure = patterns(T9_est = 'T9_est', T9_moe = 'T9_moe')
                   )

reg_tbl9_l[, `:=` (measurement_id = as.character(measurement_id),
                   table_id = 'T9_')
           ][, `Column Name` := paste0(table_id, measurement_id)]


# write to Elmer staging ----


elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes") 

sql_schema <- 'stg'

## write data dictionary to Elmer staging ----
table_id <- Id(schema = sql_schema, table = 'chas_data_dict')
dbWriteTable(conn = elmer_connection, name = table_id, value = dict_df, overwrite=TRUE)

## write Table 9 to Elmer staging ----
table_id <- Id(schema = sql_schema, table = elmer_table_name)
dbWriteTable(conn = elmer_connection, name = table_id, value = reg_tbl9_l, overwrite=TRUE)

dbDisconnect(elmer_connection)
