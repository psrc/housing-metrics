import numpy as np
import pandas as pd
import urllib
import pyodbc
from pathlib import Path
import requests
import zipfile
import sqlalchemy


conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
sql_conn = pyodbc.connect(conn_string)
params = urllib.parse.quote_plus(conn_string)
engine = sqlalchemy.create_engine("mssql+pyodbc:///?odbc_connect=%s" % params)

data_dir = Path('..\\..\\data\\PUMS\\')
year = 2019
pums_csv_name_persons = 'psam_p53.csv'

# read data from website to file



pums_base_url = 'https://www2.census.gov/programs-surveys/acs/data/pums/{}/1-Year/'.format(str(year))

# extract file




# filter data to PSRC tracts


# get the columns of data we usually look at


# wrangle the data into the shape we'd like to use for Elmer


# write to Elmer

# coding: utf-8



# In[27]:





# In[32]:



pums_doc_url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019.csv'


# In[71]:


person_zip = 'csv_pwa.zip'
pums_person_url = pums_base_url + person_zip
pums_person_file = data_dir / person_zip
housing_zip = 'csv_hwa.zip'
pums_housing_url = pums_base_url + housing_zip
pums_housing_file = data_dir / housing_zip


# In[23]:


r=requests.get(pums_person_url)
with open(pums_person_file, 'wb') as f:
    f.write(r.content)
with zipfile.ZipFile(pums_person_file, 'r') as zip_ref:
    zip_ref.extractall(data_dir)  


# In[72]:


r=requests.get(pums_housing_url)
with open(pums_housing_file, 'wb') as f:
    f.write(r.content)
with zipfile.ZipFile(pums_housing_file, 'r') as zip_ref:
    zip_ref.extractall(data_dir)   


# In[51]:


data_dict_cols = ['dd_type','colname','datatype','length', 'col_e','col_f','col_g']
df_dd = pd.read_csv(pums_doc_url, header=None, names=data_dict_cols)


# In[69]:


df_colnames = df_dd[df_dd.dd_type =='NAME']
df_vals = df_dd[df_dd.dd_type == 'VAL']


# In[130]:


person_csv_path = data_dir / 'psam_p53.csv'
df_persons = pd.read_csv(person_csv_path, dtype=object)


# In[206]:


housing_csv_path = data_dir / 'psam_h53.csv'
df_housing = pd.read_csv(housing_csv_path, dtype=object)


# In[154]:


def lookup_df(pums_col):
    df = df_vals[df_vals.colname == pums_col][['colname','datatype','length','col_e','col_f','col_g']]
    return df


# In[242]:


def data_type(pums_col):
    data_type = df_colnames[df_colnames.colname == pums_col].datatype.unique()[0]
    range_cols = df_vals[df_vals.col_e != df_vals.col_f].colname.unique()
    if data_type == 'C' and pums_col not in range_cols:
        returned_data_type = 'char_lookup'
    else:
        returned_data_type = data_type
    return returned_data_type


# In[184]:


def decode_char_column(pums_df, col_name):
    df_temp = lookup_df(col_name)
    #df_temp = df_housing.merge(df_access, how='left', left_on='ACCESS', right_on='col_e')
    pums_df[col_name] = pums_df.merge(df_temp, how='left', left_on=col_name, right_on='col_e')[['col_g']]
    return pums_df


# In[226]:


def decode_columns(pums_df):
    working_df = pums_df.copy(deep=True)
    for col in working_df.columns:
        col_type = data_type(col)
        if col_type == 'char_lookup':
            working_df_df = decode_char_column(working_df, col)  
    return working_df


# In[245]:


df_housing_decoded = decode_columns(df_housing)


# In[247]:


df_persons_decoded = decode_columns(df_persons)


# In[ ]:


# MOREMORE: Need to send the two decoded df's to Elmer


# In[272]:


def castable(df, col_name, dtype):
    try:
        df[col_name].astype(dtype)
        return True
    
    except:
        return False


# In[282]:


def get_stage_table_col_types(pums_df):
    col_types = {}
    for c in pums_df.columns:
        if castable(pums_df, c, np.int16):
            dtype = np.int16
        elif castable(pums_df, c, np.int32):
            dtype = np.int32
        elif castable(pums_df, c, np.int64):
            dtype = np.int64
        elif castable(pums_df, c, np.float64 ):
            dtype = np.float64
        else:
            dtype = object
        col_types[c] = dtype
    return col_types  


# In[283]:


df_persons_coltypes = get_stage_table_col_types(df_persons_decoded)


# In[286]:


df = pd.DataFrame({'a': [1,2,3],'b':[10,11,12]})


# In[295]:


def recast_coltypes(df):
    new_dtypes = get_stage_table_col_types(df)
    for col in new_dtypes.keys():
        df[col] = df[col].astype(new_dtypes[col])


# In[301]:


recast_coltypes(df_persons_decoded)
df_persons_decoded.dtypes


# In[302]:


def df_to_staging(df, table_name):
    recast_coltypes(df)
    df.to_sql(name=table_name, schema='stg', con=engine)


# In[ ]:


df_to_staging(df_persons_decoded, 'psam_p53_2019')

