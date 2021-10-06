# See API documentation: https://www.huduser.gov/portal/dataset/chas-api.html
# This script accesses the CHAS API to query county level summary data
# requires a personal access token stored in a text file. Modify file path on line #27.

import requests
import pandas as pd
import re

def build_url_query_param(**kwargs):
    '''
    Build the API endpoint with query parameters 
    '''
    params = []
    if('year' in kwargs):
        params.append('year=' + str(kwargs['year']))
    if('type' in kwargs):
        params.append('type=' + str(kwargs['type']))
    if('state' in kwargs):
        params.append('stateId=' + str(kwargs['state']))
    if('county' in kwargs):
        params.append('entityId=' + str(kwargs['county']))
    url = "https://www.huduser.gov/hudapi/public/chas"
    query = '?' + '&'.join(x for x in params)
    endpoint = url + query
    return(endpoint)

# store pat in txt and read-in
pat_file = pd.read_csv(r'H:\chas-pat.txt', header = None)
pat = pat_file.iloc[0][0]

# read-in file of data-dictionary
prop_dict = pd.read_csv('chas-response-prop.csv')

# request county summaries and convert to dataframe
counties = ['33', '35', '53', '61']

results = []
for county in counties:
    url = build_url_query_param(type=3, county=county, state=53)
    headers = {"Authorization": "Bearer " + pat}
    response = requests.get(url, headers = headers)
    print(response.text)
    json_data = response.json()[0] # extract dict from list
    results.append(json_data)

# wrangle results
r = [pd.DataFrame.from_dict(x, orient="index") for x in results]
df = pd.concat(r, axis=1)

# assign headers
county_names = list(df.loc['geoname'])
df.columns =  [re.search(r"^\w+", x).group(0) for x in county_names]

# join with descriptions
df.reset_index(level=0, inplace=True)
df = df.merge(prop_dict, left_on = 'index', right_on = 'property', how = 'left')
