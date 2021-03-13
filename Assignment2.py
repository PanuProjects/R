"""





# Exercise 1 instructions

## Prequisities 
* Virtual environment up and running, see instructions from repository root
* Add `secrets.py` file to exercise folder. You get the content from instructors

                    ################################### Backlogi #######################

1. - Lue data: wind power generation (ID 75) ja electricity consumption in Finland (124)
2. - Store API data in your own SQLite database
3. -  Analyze how much of the Finnish consumption can be covered with wind generated electricity.
* Store the hourly percentage of wind generated / total  consumption to your database. (See table: hourly_wind_coverage)
* Note that this database has different date schema, so you need to generate year, month, day and  hour values, too.
* For example, extracting the year can be done with pd.to_datetime(df['start_time']).dt.year
4. - Resample the hourly percentages to daily level by taking the mean on the daily level.
* Store the results in the database (table: daily_wind_coverage)
- Repeat step number four but do the grouping based on month (table: monthly_wind_coverage).
* Are the results different for different time resolutions?
* How much of the Finnish electricity consumption can be met with wind production? 


"""

# APIs here: https://data.fingrid.fi/en/
#https://data.fingrid.fi/en/dataset/wind-power-generation) and electricity consumption in Finland (https://data.fingrid.fi/en/dataset/electricity-consumption-in-finland). Their variable ids that are used in endpoints are 75 (wind) and 124 (consumption).

#fingrid_api_token ='qFXtrmshfU7X9pvBq6Zbiavr5L3IQLtu89vRb4a2'
#venv\Scripts\activate

#%%
import requests
import pandas as pd
from datetime import datetime
import json
import sqlite3

print('Packages loaded')

api_key = 'qFXtrmshfU7X9pvBq6Zbiavr5L3IQLtu89vRb4a2'
#url = 'https://data.fingrid.fi/en/' # Linkki Apiin?

#pariohjelmointi:
#solita core 2019 - vili heikkilä & niila gauriloff
#medium - pair coding 


# %% 
# 
# #1. - Lue data: wind power generation (ID 75) ja electricity consumption in Finland (124)


#API vastaa osoitteessa https://api.fingrid.fi/
#Ollaan kiinnostuneita kahdesta datasetistä: are 75 (wind) and 124 (consumption)
#Rekisteröitymisen yhteydessä saamasi API-key lisätään sellaisenaan http-kutsujen headeriin x-api-key. 
# Avainta ei tule käyttää esimerkiksi selaimen URL-osoiterivillä. Jos haet dataa suoraan johonkin sovellukseen, voit tyypillisesti määritellä sovelluksessasi API:n URL-osoitteen yhteyteen muita parametrejä ja sijoittaa avaimen siihen.


#Data timestamps are presented in UTC (prev. GMT) time format. Correct format for API requests is YYYY-MM-ddTHH:mm:ssZ
#https://data.fingrid.fi/en/
ts=datetime.now().date()


# Dates to string and formarting of beginning point
end_time = str(ts.strftime('%Y-%m-%d')+'T00:00:00Z')
start_time = str(end_time.replace('21', '20'))


#fingrid_url = 'https://data.fingrid.fi/en/json?start_time={}&end_time={}'.format(start_time, end_time)
fingrid_url_generation ='https://api.fingrid.fi/v1/variable/75/events/json?start_time={}&end_time={}'.format(start_time, end_time)
fingrid_url_consum ='https://api.fingrid.fi/v1/variable/124/events/json?start_time={}&end_time={}'.format(start_time, end_time)

#fingrid_url = 'https://api.fingrid.fi/v1/variable/75/events/json'
#fingrid_url = 'https://api.fingrid.fi/v1/variable/75/events/json??start_time=2018-01-01T00:00:00Z&end_time=2019-06-11T00:00:00Z'

#API vastaa osoitteessa https://api.fingrid.fi/...
#https://data.fingrid.fi/en/json?start_time=2020-03-12T00:00:00Z&end_time=2021-03-12T00:00:00Z
token = api_key
headers = {'x-api-key': '{}'.format(token)}
#params={'start_time': start_time, 'end_time': end_time}
response_generation = requests.get(fingrid_url_generation, headers=headers)
response_consum = requests.get(fingrid_url_consum, headers=headers)

#print(response.json())

#params={‘start_time’: dt_str2, ‘end_time’: dt_str1}
#response = requests.get(fingrid_url, headers=headers)

print('Data loaded succesfully')


    # access JSOn content
#jsonResponse = response.json()


#%%


#url = ‘https://api.fingrid.fi/v1/variable/75/events/json?start_time=2018-01-01T00:00:00Z&end_time=2019-06-11T00:00:00Z'



jsonConsum = response_consum.json()
jsonGeneration = response_generation.json()


ConsumDF=pd.DataFrame(jsonConsum)
GenerationDF=pd.DataFrame(jsonGeneration)

print('Transformed data')


#%%


"""
2. Store API data in your own SQLite database. You can use the attached .db file for a starting point.
 (Use SQLite browser to investigate tables: electricity_consumption and wind_power_generation).
  The tables’ variable names match API variable names, so writing to database can be done easily with e.g. Pandas to_sql function. 
"""

conn = sqlite3.connect('finnish_wind_power_orig.db')
c = conn.cursor()


ConsumDF.to_sql('electricity_consumption', conn, if_exists='replace', index = False)
GenerationDF.to_sql('wind_power_generation', conn, if_exists='replace', index = False)

# %%

"""
3. -  Analyze how much of the Finnish consumption can be covered with wind generated electricity.
    * Store the hourly percentage of wind generated / total  consumption to your database. (See table: hourly_wind_coverage)
    * Note that this database has different date schema, so you need to generate year, month, day and  hour values, too.
    * For example, extracting the year can be done with pd.to_datetime(df['start_time']).dt.year

"""

#%%


#%%
hourly_wind_coverage = ConsumDF

year = pd.to_datetime(ConsumDF['start_time']).dt.year
hourly_wind_coverage.insert(1, 'year', year)

month = pd.to_datetime(ConsumDF['start_time']).dt.month
hourly_wind_coverage.insert(1, 'month', month)

day = pd.to_datetime(ConsumDF['start_time']).dt.day
hourly_wind_coverage.insert(1, 'day', day)

hour = pd.to_datetime(ConsumDF['start_time']).dt.hour
hourly_wind_coverage.insert(1, 'hour', hour)

percentage =  GenerationDF['value'] / ConsumDF['value'] #oli väärin päin
hourly_wind_coverage.insert(1, 'percentage', percentage)


hourly_wind_coverage_tosql = hourly_wind_coverage.drop(['start_time', 'end_time'], axis=1)


#%%
#hourly_wind_coverage = ConsumDF


hourly_wind_coverage_tosql.to_sql('hourly_wind_coverage', conn, if_exists='replace', index = False)

#%%
"""
4. - Resample the hourly percentages to daily level by taking the mean on the daily level.
* Store the results in the database (table: daily_wind_coverage)
"""

#type(hourly_wind_coverage_tosql['day'])

hourly_wind_coverage_tosql.groupby(by=['day']).mean()