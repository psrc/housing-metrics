import numpy as np
import pandas as pd
import urllib
import pyodbc
from pathlib import Path
import requests
import zipfile
import sqlalchemy

class pums_pipeline:

  def __init__(self, year):
    try:
      self.conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
      self.sql_conn = pyodbc.connect(self.conn_string)
      params = urllib.parse.quote_plus(self.conn_string)
      self.engine = sqlalchemy.create_engine("mssql+pyodbc:///?odbc_connect=%s" % params)
      self.data_dir = Path('..\\..\\data\\PUMS\\')
      self.year = year
      self.pums_csv_name_persons = 'psam_p53.csv'
      self.pums_csv_name_housing = 'psam_h53.csv'
      self.adjust_factor_cols = ['ADJHSG', 'ADJINC']
      pums_base_url = 'https://www2.census.gov/programs-surveys/acs/data/pums/{}/1-Year/'.format(str(year))
      self.pums_doc_url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_{}.csv'.format(str(year))
      person_zip = 'csv_pwa.zip'
      self.pums_person_url = pums_base_url + person_zip
      self.pums_person_file = self.data_dir / person_zip
      housing_zip = 'csv_hwa.zip'
      self.pums_housing_url = pums_base_url + housing_zip
      self.pums_housing_file = self.data_dir / housing_zip
      self.data_dict_cols = ['dd_type','colname','datatype','length', 'col_e','col_f','col_g']

    except Exception as e:
      raise


  def download_and_extract_files(self):

    try:
      def dwnld(url, a_file):
        r=requests.get(url)
        with open(a_file, 'wb') as f:
          f.write(r.content)
        with zipfile.ZipFile(a_file, 'r') as zip_ref:
          zip_ref.extractall(self.data_dir)

      dwnld(self.pums_person_url, self.pums_person_file)
      dwnld(self.pums_housing_url, self.pums_housing_file)

    except Exception as e:
      raise

  def read_data_dicts(self):

    try:
      df_dd = pd.read_csv(self.pums_doc_url, header=None, names=self.data_dict_cols) 
      self.df_colnames = df_dd[df_dd.dd_type == 'NAME']
      self.df_vals = df_dd[df_dd.dd_type == 'VAL']

    except Exception as e:
      raise
  

  def download_data(self):

    try:
      person_csv_path = self.data_dir / self.pums_csv_name_persons
      self.df_persons = pd.read_csv(person_csv_path, dtype=object)
      housing_csv_path = self.data_dir / self.pums_csv_name_housing
      self.df_housing = pd.read_csv(housing_csv_path, dtype=object)

    except Exception as e:
      raise

  def lookup_df(self, pums_col):

    try:
      df_vals = self.df_vals
      df = df_vals[df_vals.colname == pums_col][['colname','datatype','length','col_e','col_f','col_g']]
      return df

    except Exception as e:
      raise

  def data_type(self, pums_col):

    try:
      df_colnames = self.df_colnames
      df_vals = self.df_vals
      data_type = df_colnames[df_colnames.colname == pums_col].datatype.unique()[0]
      range_cols = df_vals[df_vals.col_e != df_vals.col_f].colname.unique()
      if data_type == 'C' and pums_col not in range_cols and pums_col not in self.adjust_factor_cols:
          returned_data_type = 'char_lookup'
      else:
          returned_data_type = data_type
      return returned_data_type

    except Exception as e:
      raise

  def decode_char_column(self, pums_df, col_name):

    try:
      df_temp = self.lookup_df(col_name)
      #df_temp = df_housing.merge(df_access, how='left', left_on='ACCESS', right_on='col_e')
      pums_df[col_name] = pums_df.merge(df_temp, how='left', left_on=col_name, right_on='col_e')[['col_g']]
      return pums_df

    except Exception as e:
      raise

  def decode_columns(self, pums_df):

    try:
      working_df = pums_df.copy(deep=True)
      for col in working_df.columns:
          col_type = self.data_type(col)
          if col_type == 'char_lookup':
              working_df_df = self.decode_char_column(working_df, col)  
      return working_df

    except Exception as e:
      raise

  def castable(self, df, col_name, dtype):

    try:
        df[col_name].astype(dtype)
        return True

    except:
        return False


  def get_stage_table_col_types(self, pums_df):

    try:
      col_types = {}
      for c in pums_df.columns:
          if self.castable(pums_df, c, np.int16):
              dtype = np.int16
          elif self.castable(pums_df, c, np.int32):
              dtype = np.int32
          elif self.castable(pums_df, c, np.int64):
              dtype = np.int64
          elif self.castable(pums_df, c, np.float64 ):
              dtype = np.float64
          else:
              dtype = object
          col_types[c] = dtype
      return col_types

    except Exception as e:
      raise

  def recast_coltypes(self, df):

    try:
      new_dtypes = self.get_stage_table_col_types(df)
      for col in new_dtypes.keys():
          if col in self.adjust_factor_cols:
              df[col] = pd.to_numeric(df[col])
          else:
              df[col] = df[col].astype(new_dtypes[col])

    except Exception as e:
      raise

  def df_to_staging(self, df, table_name):

    try:
      self.recast_coltypes(df)
      df.to_sql(name=table_name, schema='stg', con=engine, if_exists='replace')

    except Exception as e:
      raise


  def execute_sql(self, sql):

    try:
      crsr = self.sql_conn.cursor()
      crsr.execute(sql)
      self.sql_conn.commit()

    except Exception as e:
      raise

  def update_meta_tables(self, pums_type):

    try:
      str_year = str(self.year)
      sql_statement = """
        with dd as (
          select distinct colname, col_e as col_definition
          from stg.pums_persons_dd
        )
        update meta.[columns]
        set [description] = dd.col_definition
        from meta.[columns] c
          join meta.[tables] t ON c.table_id = t.table_id
          join dd on c.[name] = dd.colname
        where t.[name] = 'pums_{}_{}'
      """.format(pums_type, str_year)
      self.execute_sql(sql_statement)

    except Exception as e:
      raise

  def main(self):

    try:
      self.download_and_extract_files()
      self.read_data_dicts()
      self.download_data()
      df_housing_decoded = self.decode_columns(self.df_housing)
      df_persons_decoded = self.decode_columns(self.df_persons)
      self.df_to_staging(df_persons_decoded, 'psam_p53_{}'.format(str(self.year)))
      self.df_to_staging(df_housing_decoded, 'psam_h53_{}'.format(str(self.year)))
      self.execute_sql('exec census.merge_pums_persons_{}'.format(str(self.year)))
      self.execute_sql('exec census.merge_pums_housing_{}'.format(str(self.year)))
      self.update_meta_tables('persons')
      self.update_meta_tables('housing')

    except Exception as e:
      raise
