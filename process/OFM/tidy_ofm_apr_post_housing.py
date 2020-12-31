# This script will tidy downloaded post-censal OFM April 1 housing data
import os
import pandas as pd
from pandas.api.types import is_string_dtype
import numpy as np

data_dir = '../../data'

filename = 'ofm_april1_housing.xlsx'
wa = pd.read_excel(os.path.join(data_dir, filename), sheet_name = 'Housing Units', skiprows = 3)

# filter for region
counties = ['King', 'Kitsap', 'Pierce', 'Snohomish']
reg = wa[wa['County'].isin(counties)]
    
# remove Line column, Filters 1 & 3
reg = reg[~reg['Filter'].isin([1,3])]
reg = reg.drop(columns = ['Line', 'Filter'])
    
# pivot longer
reg_pvt = reg.melt(id_vars=['County', 'Jurisdiction'], var_name='desc')
reg_pvt['estimate_year'] = reg_pvt['desc'].str.extract('(^\\d+)')
reg_pvt['housing_type'] = reg_pvt['desc'].str.extract('((?<=of\\s).*)')
reg_pvt['housing_type'] = reg_pvt['housing_type'].str.rstrip()
reg_pvt = reg_pvt.drop(columns = ['desc'])
reg_pvt['housing_type'] = reg_pvt['housing_type'].str.lower()
reg_pvt['housing_type'] = reg_pvt['housing_type'].str.replace(' ', '_')

# rename and reorder
reg_pvt.columns = map(str.lower, reg_pvt.columns)

# convert to numeric cols
if is_string_dtype(reg_pvt['value']):
    reg_pvt['value'] = np.array(reg_pvt['value'], dtype=float)

df = reg_pvt.pivot_table(index=['county', 'jurisdiction', 'estimate_year'], columns='housing_type', values='value').reset_index()

if {'moble_homes_and_specials'}.issubset(df.columns):
    df = df.rename(columns = {'moble_homes_and_specials': 'mobile_homes_and_specials'})

df.columns.name = None

cols = ['county', 'jurisdiction', 'estimate_year', 'total_housing_units', 'one_unit_housing_units', 'two_or_more_unit_housing_units', 'mobile_homes_and_specials']
df = df[cols]