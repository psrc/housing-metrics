# This script will tidy downloaded post-censal OFM April 1 data
source(here::here('process', 'OFM', 'functions_tidy.R'))

data_dir <- 'data'
filename <- 'ofm_april1_population_final.xlsx'
wa <- read.xlsx(here(data_dir, filename), startRow = 5)
df_post <-  tidy_ofm_apr(wa, 'Population')

rm(wa)