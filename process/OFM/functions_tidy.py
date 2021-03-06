import pandas as pd
from pandas.api.types import is_string_dtype
import numpy as np

def tidy_ofm_apr(dataset, tabname):
    """
    This function will clean and transform a single sheet either post-censal or intercensal April 1 data from OFM. 
    Intercensal data has a different format than the postcensal (e.g. additional Place Code columns, column names, Jurisdiction string format) 
    """
    tabname = str.lower(tabname)
    tabname = str.replace(tabname, " ", '_')
    if tabname == 'population':
        tabname = 'total_population'
    if tabname == 'gq_population':
        tabname = 'group_quarters_population'
    if tabname == 'occupied_housing':
        tabname = 'occupied_housing_units'
    if tabname == 'total_housing':
        tabname = 'housing_units'

    # Intercensal specific munging
    if {'County Name', 'City Name'}.issubset(dataset.columns):
        dataset = dataset.rename(columns = {'County Name': 'County'})
        dataset['Jurisdiction'] = dataset['Jurisdiction'].str.replace('city', '')
        dataset['Jurisdiction'] = dataset['Jurisdiction'].str.replace('town','')
        dataset['Jurisdiction'] = dataset['Jurisdiction'].replace('\\s+', ' ', regex=True)
        dataset['Jurisdiction'] = dataset['Jurisdiction'].replace('\\s$', '', regex=True)
        drop_cols = ['City Name'] + list(dataset.columns[dataset.columns.str.contains('Code$')])
        dataset = dataset.drop(columns = drop_cols)

    # filter for region
    counties = ['King', 'Kitsap', 'Pierce', 'Snohomish']
    reg = dataset[dataset['County'].isin(counties)]
    
    # remove Line column, Filters 1 & 3
    reg = reg[~reg['Filter'].isin([1,3])]
    reg = reg.drop(columns = ['Line', 'Filter'])
    
    # pivot longer
    reg_pvt = reg.melt(id_vars=['County', 'Jurisdiction'], var_name='desc', value_name=tabname)
    reg_pvt['estimate_year'] = reg_pvt['desc'].str.extract('(^\\d+)')
    reg_pvt = reg_pvt.drop(columns = ['desc'])

    # rename and reorder
    reg_pvt.columns = map(str.lower, reg_pvt.columns)
    cols = ['county', 'jurisdiction', 'estimate_year', tabname]
    df = reg_pvt[cols]

    # convert to numeric cols
    if is_string_dtype(df[tabname]):
        df[tabname] = np.array(df[tabname], dtype=float)

    return(df)
