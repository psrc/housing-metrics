# This script will tidy downloaded post-censal OFM April 1 data
import os

exec(open('functions_tidy.py').read())

data_dir = '../../data'

filename = 'ofm_april1_population_final.xlsx'
wa = pd.read_excel(os.path.join(data_dir, filename), skiprows = 4)
df_post = tidy_ofm_apr(wa, 'Population')
