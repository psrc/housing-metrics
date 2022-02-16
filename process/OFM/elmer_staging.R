library(here)

source(here('process', 'OFM', 'config.R'))

source(here('process', 'OFM', 'tidy_ofm_apr_inter.R'))
# df_inter

source(here('process', 'OFM', 'tidy_ofm_apr_post.R'))
# df_post

source(here('process', 'OFM', 'tidy_ofm_apr_post_housing.R'))
# df_post_housing