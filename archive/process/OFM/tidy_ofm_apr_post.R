# This script will tidy downloaded post-censal OFM April 1 data

source(here::here('process', 'OFM', 'config.R'))
source(here::here('process', 'OFM', 'functions_tidy.R'))

data_dir <- 'data'
filename <- filename_post
wa <- read.xlsx(here(data_dir, filename), startRow = 5)
df_post <-  tidy_ofm_apr(wa, 'Population')

rm(wa)