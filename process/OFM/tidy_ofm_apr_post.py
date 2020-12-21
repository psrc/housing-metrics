import os
import pandas as pd
import numpy as np

pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

data_dir = '../../data'
filename = 'ofm_april1_population_final.xlsx'

wa = pd.read_excel(os.path.join(data_dir, filename), skiprows = 4)

# filter for region
counties = ['King', 'Kitsap', 'Pierce', 'Snohomish']
reg = wa[wa['County'].isin(counties)]

# remove Line column, Filters 1 & 3
reg = reg[~reg['Filter'].isin([1,3])]
reg = reg.drop(columns = ['Line', 'Filter'])

# pivot longer
reg_pvt = reg.melt(id_vars=['County', 'Jurisdiction'], var_name='desc', value_name='total_population')
reg_pvt['estimate_year'] = reg_pvt['desc'].str.extract('(^\\d+)')
reg_pvt = reg_pvt.drop(columns = ['desc'])

# rename and reorder
reg_pvt.columns = map(str.lower, reg_pvt.columns)
cols = ['county', 'jurisdiction', 'estimate_year', 'total_population']
df = reg_pvt[cols]

# convert to numeric cols
df['total_population'] = pd.to_numeric(df['total_population'])


