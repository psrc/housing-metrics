import numpy as np
import pandas as pd
import urllib
import pyodbc
import sqlalchemy
from pathlib import Path

data_dir = Path('..\\..\\data\\subareas')
excel_filename = 'Tract_Subarea_Crosswalk.xlsx'
conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
sql_conn = pyodbc.connect(conn_string)
params = urllib.parse.quote_plus(conn_string)
engine = sqlalchemy.create_engine("mssql+pyodbc:///?odbc_connect=%s" % params)

def get_df():
    filename = data_dir / excel_filename
    df = pd.read_excel(filename, sheet_name="Sheet1", dtype='object')
    return df

def port_to_staging(df, engine):
    try:
        df.to_sql(name='tract_subareas', schema='stg', con=engine, if_exists='replace')
    except Exception as e:
        raise

df = get_df()
port_to_staging(df, engine)
