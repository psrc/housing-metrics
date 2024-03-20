# This script will tidy and compile downloaded intercensal OFM April 1 data

source(here::here('process', 'OFM', 'config.R'))
source(here::here('process', 'OFM', 'functions_tidy.R'))

data_dir <- 'data'
filename <- filename_inter

tabs <- getSheetNames(here(data_dir, filename))
tabs <- tabs[which(tabs != 'Readme')]
all <- map(tabs, ~read.xlsx(xlsxFile = here(data_dir, filename), sheet = .x))

names(all) <- tabs

# tidy all dataframes in the workbook
# join all dataframes into one
join_dfs <- partial(merge, on = c('county', 'jurisdiction', 'estimate_year'))
df_inter <- map2(all, tabs, ~tidy_ofm_apr(.x, .y)) %>% 
  reduce(join_dfs)

rm(all)

