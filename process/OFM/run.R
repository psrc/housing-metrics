# Step 1 ----

source(here::here('process', 'OFM', 'functions_download_ofm_apr.R'))
# download_ofm_apr1_est('inter', start = '2000', end = '2010')
# download_ofm_apr1_est('post', posttype = 'population')
# download_ofm_apr1_est('post', posttype = 'housing')

# Step 2 ----

# Update config.R file with file names and save.
# Make sure data is located the housing-metrics/data sub-folder

# Step 3 ----

source(here::here('process', 'OFM', 'functions_elmer_staging.R'))
# data_to_elmer('intercensal')
# data_to_elmer('postcensal')
# data_to_elmer('postcensal housing')
