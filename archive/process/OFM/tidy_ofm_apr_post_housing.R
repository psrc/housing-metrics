# This script will tidy downloaded post-censal OFM April 1 housing data

source(here::here('process', 'OFM', 'config.R'))
source(here::here('process', 'OFM', 'functions_tidy.R'))

data_dir <- 'data'
filename <- filename_post_housing
wa <- read.xlsx(here(data_dir, filename), sheet = 'Housing Units', startRow = 4)
df_post_housing <-  tidy_ofm_apr(wa, 'Housing Units')

rm(wa)
