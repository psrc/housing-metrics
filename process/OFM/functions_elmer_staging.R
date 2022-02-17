library(here)
library(odbc)
library(DBI)

data_to_elmer <- function(data.type) {
  elmer_connection <- dbConnect(odbc::odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\Sockeye",
                                database = "Elmer",
                                trusted_connection = "yes"
  ) 
  
  sql_schema = "stg"
  
  if(data.type == 'intercensal') {
    source(here('process', 'OFM', 'tidy_ofm_apr_inter.R'))
    sql_table_name <-  "ofm_apr_intercensal"
    df <- df_inter
    
  } else if(data.type == 'postcensal') {
    source(here('process', 'OFM', 'tidy_ofm_apr_post.R'))
    sql_table_name <-  "ofm_apr_postcensal"
    df <- df_post
    
  } else if(data.type == 'postcensal housing') {
    source(here('process', 'OFM', 'tidy_ofm_apr_post_housing.R'))
    sql_table_name <-  "ofm_apr_postcensal_housing"
    df <- df_post_housing
  }
  
  table_id <- Id(schema = sql_schema, table = sql_table_name)
  dbWriteTable(conn = elmer_connection, name = table_id, value = df, overwrite=TRUE)
  dbDisconnect(elmer_connection)
  print(paste0('Data exported to Elmer, please double check table in Elmer (schema: stg; tablename: ', sql_table_name, ').'))
}
