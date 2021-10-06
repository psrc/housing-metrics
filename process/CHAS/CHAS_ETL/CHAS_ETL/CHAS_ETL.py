import numpy as np
import pandas as pd
import urllib
import pyodbc
from pathlib import Path
import requests
import zipfile
import sqlalchemy

# This script reads CHAS data from the HUD website, downloads it, unzips it;
#then it puts it in the Elmer staging database

# data website is here:https://www.huduser.gov/portal/datasets/cp.html
# background about data is here: http://aws-linux/mediawiki/index.php/Comprehensive_Housing_Affordability_Strategy_(CHAS)

# functions to help write to the staging database
def castable(df, col_name, dtype):
    try:
        df[col_name].astype(dtype)
        return True
    
    except:
        return False

def get_stage_table_col_types(df):
    col_types = {}
    for c in df.columns:
        if castable(df, c, np.int16):
            dtype = np.int16
        elif castable(df, c, np.int32):
            dtype = np.int32
        elif castable(df, c, np.int64):
            dtype = np.int64
        elif castable(df, c, np.float64 ):
            dtype = np.float64
        else:
            dtype = object
        col_types[c] = dtype
    return col_types  


def recast_coltypes(df):
    print('recasting column types...')
    new_dtypes = get_stage_table_col_types(df)
    for col in new_dtypes.keys():
        df[col] = df[col].astype(new_dtypes[col])
    print('finished recasting column types.')


def df_to_staging(df, table_name):
    print('writing {} to stg schema...'.format(table_name))
    recast_coltypes(df)
    df.to_sql(name=table_name, schema='stg', con=engine, if_exists='replace')
    print('finished writing {} to stg schema'.format(table_name))


############### Paths and Database parameters

# url to read from
base_url = 'https://www.huduser.gov/portal/datasets/cp/'
data_file_name = '2014thru2018-140-csv.zip'
url_for_file = base_url+data_file_name

#zip file to write to
# data_dir = 'C:\\Users\\SChildress\\Documents\\GitHub\\housing-metrics\\data\\'
data_dir = '..\\..\\..\\..\\data\\'
file_name_local = data_dir+data_file_name
output_path_zip = data_dir+'\\140\\'

# extracted file names for table and dictionary
table_9_file_name = 'Table9.csv'
table_9_path_name = output_path_zip+table_9_file_name
data_dict_name = 'CHAS data dictionary 14-18.xlsx'
data_dict_path_name = output_path_zip+data_dict_name

# SQL Server info
conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
sql_conn = pyodbc.connect(conn_string)
params = urllib.parse.quote_plus(conn_string)
engine = sqlalchemy.create_engine("mssql+pyodbc:///?odbc_connect=%s" % params)

########## Go get the data and unzip

# read data from website to file, extract
r=requests.get(url_for_file)
with open(file_name_local, 'wb') as f:
    f.write(r.content)
with zipfile.ZipFile(file_name_local, 'r') as zip_ref:
    zip_ref.extractall(data_dir)  

#We use tract-level data for CHAS Table 9 to produce Cost Burden by Tenure (Owner, Renter) 
#and Race/Ethnicity, along with an in-house crosswalk table between census tracts and county subareas 
#“Cost burden” is defined as monthly housing costs as a percentage of gross income; t
##here are 4 cost burden categories: 1) less than or equal to 30%, 2) greater than 30% but less than or equal to 50%, 3) greater than 50%, and 4) cost burden not computed (no or negative income) 
#Race/ethnicity categories include: 1) White alone, non-Hispanic, 2) Black or African-American alone, non-Hispanic, 3) Asian alone, non-Hispanic, 4) American Indian or Alaska Native alone, non-Hispanic, 5) Pacific Islander alone, non-Hispanic, 6) Hispanic, any race, and 7) Other (including multiple races, non-Hispanic).
#Basically, the first step involves pulling the relevant data fields from CHAS Table 9 
#for each census tract in the region.

#  read in table 9 info Cost Burden by Tenure and Race, also read data dictionary

################ Read the data from excel/csv, wrangle it into a good shape, put it in elmer


data_dict_df = pd.read_excel(data_dict_path_name, sheet_name='Table 9')
data_dict_df['Column Name'] = data_dict_df['Column Name'].str.replace('est','')


df_to_staging(data_dict_df, 'chas_data_dict')

table_9_data = pd.read_csv(table_9_path_name, encoding = 'ISO-8859-1')
# filter data to PSRC tracts
# parse column D "name" to find which rows are for our region
counties_region = 'King County, Washington|Kitsap County, Washington|Pierce County, Washington|Snohomish County, Washington'
region_table_9 = table_9_data[table_9_data['name'].str.contains(counties_region)]

# truncate geo_id to remove the part before US so it can join on our usual tract data

region_table_9['GEOID_short'] = region_table_9['geoid'].str.split('US').str[1]
region_table_9["id"] = region_table_9.index

# pivot table to be long for each estimate, moe

table_9_data_long = pd.wide_to_long(region_table_9,['T9_est', 'T9_moe'], i='id',j='measurement_id').reset_index()
table_9_data_long['measurement_id']=table_9_data_long['measurement_id'].astype(str)
table_9_data_long['table_id']='T9_'
table_9_data_long['Column Name'] = table_9_data_long[['table_id','measurement_id']].apply(lambda x: ''.join(x), axis=1)


df_to_staging(table_9_data_long, 'chas_tbl_9_2018')

