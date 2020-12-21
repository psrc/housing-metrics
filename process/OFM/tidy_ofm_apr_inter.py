# This script will tidy downloaded intercensal OFM April 1 data

import os

exec(open('functions_tidy.py').read())

pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

data_dir = '../../data'

filename = 'ofm_april1_population_intercensal.xlsx'

atabname = 'Total Population'
wa = pd.read_excel(os.path.join(data_dir, filename), sheet_name=atabname)

df = tidy_ofm_apr(wa, atabname)

