import urllib
import pyodbc
import sqlalchemy

sql_schema = "stg"

conn_string = "DRIVER={ODBC Driver 17 for SQL Server}; SERVER=AWS-PROD-SQL\Sockeye; DATABASE=Elmer; trusted_connection=yes"
sql_conn = pyodbc.connect(conn_string)
params = urllib.parse.quote_plus(conn_string)
engine = sqlalchemy.create_engine("mssql+pyodbc:///?odbc_connect=%s" % params)

# intercensal data
exec(open('tidy_ofm_apr_inter.py').read())
sql_table_name = "ofm_apr_intercensal"
df_inter.to_sql(name=sql_table_name, schema=sql_schema, con=engine)

# post-censal data
exec(open('tidy_ofm_apr_post.py').read())
sql_table_name = "ofm_apr_postcensal"
df_post.to_sql(name=sql_table_name, schema=sql_schema, con=engine)
