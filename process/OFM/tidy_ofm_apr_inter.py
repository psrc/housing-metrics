# This script will tidy and compile downloaded intercensal OFM April 1 data
import os
from functools import partial, reduce

exec(open('functions_tidy.py').read())

data_dir = '../../data'
filename = 'ofm_april1_population_intercensal.xlsx'

# Read in entire workbook as dictionary
all = pd.read_excel(os.path.join(data_dir, filename), sheet_name=None)
del all['Readme']

# tidy all dataframes in the workbook
df_all = list()
for tabname in list(all.keys()):
    temp_df = tidy_ofm_apr(all[tabname], tabname)
    df_all.append(temp_df)

# join all dataframes into one
join_dfs = partial(pd.merge, on = ['county', 'jurisdiction', 'estimate_year'])
df = reduce(join_dfs, df_all)
