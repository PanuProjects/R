#%%

import pandas as pd
import requests as rq
from datetime import datetime


print('Packages Loaded')

#%%

#https://api.fingrid.fi/v1/variable/75/events/json?start_time=2018-01-01T00:00:00Z&end_time=2019-06-11T00:00:00Z, jossa 75 on variableId
#eli tässä tapauksessa tuulivoiman tuotanto.


# Lets take current date end point for timeline
ts=datetime.now().date()


# Dates to string and formarting of beginning point
dt_str1 = ts.strftime('%Y-%m-%d')+'T00:00:00Z'
dt_str2 = dt_str1.replace('21', '20')

print(dt_str1)
print(dt_str2)

# %%


url = https://api.fingrid.fi/v1/variable/75/events/json?start_time=2018-01-01T00:00:00Z&end_time=2019-06-11T00:00:00Z'
url=’https://api.fingrid.fi/v1/variable/75/events/json'
headers ={‘x-api-key’: api_key_fingrid}
params={‘start_time’: dt_str2, ‘end_time’: dt_str1}
response = requests.get(url, headers=headers,params=params)
# print(response.text)
data=json.loads(response.text)
fg_df=pd.DataFrame(data)