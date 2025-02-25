# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.6
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# ## **SIG731 : Task 6D: pandas vs SQL**
#
# #### **Name: Isha Shukla**
# #### Student number: 225170943
# #### E-mail - isha03shukla04@gmail.com

# %% [markdown]
# ### **Introduction**  
# This task focuses on comparing **SQL queries** with their equivalent **Pandas operations** using the `nycflights13` dataset. The dataset contains information on **336,776 flights** that departed from New York airports (EWR, JFK, LGA) in 2013, including details about aircraft, airlines, weather, and departure schedules.  
#
# ##### The objective is to:  
# 1. Load the dataset into **SQLite** and execute SQL queries.  
# 2. Implement equivalent **Pandas** solutions without using SQL.  
# 3. Validate results using `pd.testing.assert_frame_equal()`.  
# 4. Compare the efficiency of SQL and Pandas operations.   

# %%
import sqlite3
import pandas as pd

# %%
# Connect to SQLite database
conn = sqlite3.connect("flights.db")
cursor = conn.cursor()

# Load CSV files into Pandas
flights = pd.read_csv("/Users/ishashukla/Desktop/Deakin Uni/SIG731- Data Wrangling/data_files/nycflights13_flights.csv.gz",comment="#")
airlines = pd.read_csv("/Users/ishashukla/Desktop/Deakin Uni/SIG731- Data Wrangling/data_files/nycflights13_airlines.csv.gz",comment="#")
airports = pd.read_csv('/Users/ishashukla/Desktop/Deakin Uni/SIG731- Data Wrangling/data_files/nycflights13_airports.csv.gz',comment="#")
planes = pd.read_csv('/Users/ishashukla/Desktop/Deakin Uni/SIG731- Data Wrangling/data_files/nycflights13_planes.csv.gz',comment="#")
weather = pd.read_csv('/Users/ishashukla/Desktop/Deakin Uni/SIG731- Data Wrangling/data_files/nycflights13_weather.csv.gz',comment="#")

# Save to SQLite database
flights.to_sql("flights", conn, if_exists="replace", index=False)
airlines.to_sql("airlines", conn, if_exists="replace", index=False)
airports.to_sql("airports", conn, if_exists="replace", index=False)
planes.to_sql("planes", conn, if_exists="replace", index=False)
weather.to_sql("weather", conn, if_exists="replace", index=False)

# %% [markdown]
# ### **1. SELECT DISTINCT engine FROM planes**

# %% [markdown]
# #### Query 1: DISTINCT(Unique, removing duplicates) engine types FROM table planes

# %%
task1_sql = pd.read_sql_query("SELECT DISTINCT engine FROM planes;", conn)

# %%
task1_sql

# %%
task1_pandas = planes[['engine']].drop_duplicates().reset_index(drop=True)

# %%
task1_pandas

# %%
pd.testing.assert_frame_equal(task1_sql, task1_pandas)

# %% [markdown]
# ### **2. SELECT DISTINCT type, engine FROM planes**

# %% [markdown]
# #### Query 2: DISTINCT(unique, by removing duplicates) type and engine FROM table planes

# %%
task2_sql = pd.read_sql_query("SELECT DISTINCT type, engine FROM planes;", conn)
task2_sql

# %%
task2_pandas = planes[['type', 'engine']].drop_duplicates().reset_index(drop=True)
task2_pandas

# %%
pd.testing.assert_frame_equal(task2_sql, task2_pandas)

# %% [markdown]
# ### **3. SELECT COUNT(*), engine FROM planes GROUP BY engine**

# %% [markdown]
# #### Query 3: Count of planes per engine type FROM table planes

# %%
task3_sql = pd.read_sql_query("SELECT COUNT(*), engine FROM planes GROUP BY engine;", conn)

# %%
task3_pandas = planes.groupby('engine').size().reset_index(name='COUNT(*)')
task3_pandas = task3_pandas[['COUNT(*)', 'engine']]
task3_pandas

# %%
pd.testing.assert_frame_equal(task3_sql, task3_pandas)

# %% [markdown]
# ### **4. SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type**

# %% [markdown]
# #### Query 4: Count of planes per engine and type

# %%
task4_sql = pd.read_sql_query("SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type",conn)

# %%
task4_pandas = planes.groupby(['engine','type']).size().reset_index(name='COUNT(*)')
task4_pandas = task4_pandas[['COUNT(*)','engine','type']]
task4_pandas

# %%
pd.testing.assert_frame_equal(task4_sql, task4_pandas)

# %% [markdown]
# ### **5. SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes GROUP BY engine, manufacturer**

# %% [markdown]
# #### Query 5: MIN, AVG, MAX year grouped by engine and manufacturer FROM table planes

# %%
task5_sql = pd.read_sql_query("SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes GROUP BY engine, manufacturer"
                              ,conn)

# %%
task5_pandas = planes.groupby(['engine', 'manufacturer'])['year'].agg(['min', 'mean', 'max']).reset_index()
task5_pandas.columns = ['engine', 'manufacturer', 'MIN(year)', 'AVG(year)', 'MAX(year)']
task5_pandas

# %%
task5_pandas = task5_pandas[['MIN(year)', 'AVG(year)', 'MAX(year)', 'engine', 'manufacturer']] #changing the position of columns
task5_pandas

# %%
pd.testing.assert_frame_equal(task5_sql, task5_pandas)

# %% [markdown]
# ### **6. SELECT * FROM planes WHERE speed IS NOT NULL**

# %% [markdown]
# #### Query 6: Planes with non-null speed values

# %%
task6_sql = pd.read_sql_query("SELECT * FROM planes WHERE speed IS NOT NULL;", conn)

# %%
task6_pandas = planes[planes['speed'].notnull()].reset_index(drop=True)
task6_pandas

# %%
pd.testing.assert_frame_equal(task6_sql, task6_pandas)

# %% [markdown]
# ### **7. SELECT tailnum FROM planes WHERE seats BETWEEN 150 AND 210 AND year >= 2011**

# %%
task7_sql = pd.read_sql_query("SELECT tailnum FROM planes WHERE seats BETWEEN 150 AND 210 AND year >= 2011;", conn)

# %%
task7_pandas = planes[(planes['seats'].between(150, 210)) & (planes['year'] >= 2011)][['tailnum']].reset_index(drop=True)
task7_pandas

# %%
pd.testing.assert_frame_equal(task7_sql, task7_pandas)

# %% [markdown]
# ### **8. SELECT tailnum, manufacturer, seats FROM planes WHERE manufacturer IN ("BOEING", "AIRBUS", "EMBRAER") AND seats>390**

# %% [markdown]
# #### Query 8: Planes manufacturers from Boeing, Airbus, or Embraer with seats > 390

# %%
task8_sql = pd.read_sql_query("""
    SELECT tailnum, manufacturer, seats FROM planes 
    WHERE manufacturer IN ('BOEING', 'AIRBUS', 'EMBRAER') AND seats > 390;
""", conn)

# %%
task8_pandas = planes[
(planes['manufacturer'].isin(['BOEING', 'AIRBUS', 'EMBRAER'])) & (planes['seats'] > 390)][['tailnum', 'manufacturer', 'seats']].reset_index(drop=True)
task8_pandas

# %%
pd.testing.assert_frame_equal(task8_sql, task8_pandas)

# %% [markdown]
# ### **9. SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY year ASC, seats DESC**

# %% [markdown]
# #### Query 9: DISTINCT year and seats for planes (year ≥ 2012) ordered by year (ASC) and seats (DESC)

# %%
task9_sql = pd.read_sql_query("""SELECT DISTINCT year, seats FROM planes
WHERE year >= 2012 ORDER BY year ASC, seats DESC""",conn)

# %%
task9_pandas = planes[planes['year'] >= 2012][['year', 'seats']].drop_duplicates().sort_values(['year', 'seats'], ascending=[True, False]).reset_index(drop=True)
task9_pandas

# %%
pd.testing.assert_frame_equal(task9_sql, task9_pandas)

# %% [markdown]
# ### **10. SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY year ASC, seats DESC**

# %% [markdown]
# #### Query 10: DISTINCT(unique) year and seats for planes (year >= 2012) ordered by seats (DESC) and year (ASC)

# %%
task10_sql = pd.read_sql_query("""
    SELECT DISTINCT year, seats FROM planes 
    WHERE year >= 2012 ORDER BY seats DESC, year ASC;
""", conn)

# %%
task10_pandas = planes[planes['year'] >= 2012][['year', 'seats']].drop_duplicates().sort_values(['seats', 'year'], ascending=[False, True]).reset_index(drop=True)
task10_pandas

# %%
pd.testing.assert_frame_equal(task10_sql, task10_pandas)

# %% [markdown]
# ### **11. SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer**

# %% [markdown]
# #### Query 11: Manufacturer-wise count of planes where seats > 200

# %%
task11_sql = pd.read_sql_query("""
    SELECT manufacturer, COUNT(*) FROM planes 
    WHERE seats > 200 GROUP BY manufacturer;
""", conn)

# %%
task11_pandas = planes[planes['seats'] > 200].groupby('manufacturer').size().reset_index(name='COUNT(*)')
task11_pandas

# %%
pd.testing.assert_frame_equal(task11_sql, task11_pandas)

# %% [markdown]
# ### **12. SELECT manufacturer, COUNT( * ) FROM planes GROUP BY manufacturer HAVING COUNT(*) > 10**

# %% [markdown]
# #### Query 12: Manufacturer-wise count of planes where count > 10

# %%
task12_sql = pd.read_sql_query("""
    SELECT manufacturer, COUNT(*) FROM planes 
    GROUP BY manufacturer HAVING COUNT(*) > 10;
""", conn)

# %%
task12_pandas = planes.groupby('manufacturer').size().reset_index(name='COUNT(*)')
task12_pandas = task12_pandas[task12_pandas['COUNT(*)'] > 10].reset_index(drop=True)
task12_pandas

# %%
pd.testing.assert_frame_equal(task12_sql, task12_pandas)

# %% [markdown]
# ### **13. SELECT manufacturer, COUNT( * ) FROM planes WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10**

# %% [markdown]
# #### Query 13: Manufacturer-wise count of planes where seats > 200 and count > 10

# %%
task13_sql = pd.read_sql_query("""
    SELECT manufacturer, COUNT(*) FROM planes 
    WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10;
""", conn)

# %%
task13_pandas = planes[planes['seats'] > 200].groupby('manufacturer').size().reset_index(name='COUNT(*)')
task13_pandas = task13_pandas[task13_pandas['COUNT(*)'] > 10].reset_index(drop=True)
task13_pandas

# %%
pd.testing.assert_frame_equal(task13_sql, task13_pandas)

# %% [markdown]
# ### **14. SELECT manufacturer, COUNT(*) AS howmany FROM planes GROUP BY manufacturer ORDER BY howmany DESC LIMIT 10**

# %% [markdown]
# #### Query 14: Top 10 manufacturers with the highest number of planes

# %%
task14_sql = pd.read_sql_query("""
    SELECT manufacturer, COUNT(*) AS howmany FROM planes GROUP BY manufacturer ORDER BY howmany DESC LIMIT 10;
""", conn)

# %%
task14_pandas = planes.groupby('manufacturer').size().reset_index(name="howmany").sort_values('howmany',ascending=False).reset_index(drop=True).head(10)
task14_pandas

# %%
pd.testing.assert_frame_equal(task14_sql, task14_pandas)

# %% [markdown]
# ### **15. SELECT flights.*, planes.year AS plane_year, planes.speed AS plane_speed, planes.seats AS plane_seats FROM flights LEFT JOIN planes ON flights.tailnum=planes.tailnum**

# %% [markdown]
# #### Query 15: LEFT JOIN flights and planes on tailnum

# %%
task15_sql = pd.read_sql_query("""SELECT flights.*, planes.year AS plane_year, planes.speed AS plane_speed,
planes.seats AS plane_seats
FROM flights LEFT JOIN planes ON flights.tailnum=planes.tailnum""",conn)

# %%
task15_pandas = flights.merge(planes[['tailnum', 'year', 'speed', 'seats']], on='tailnum', how='left')
task15_pandas.rename(columns={'year_x': 'year','year_y': 'plane_year', 'speed': 'plane_speed', 'seats': 'plane_seats'}, inplace=True)
task15_pandas

# %%
pd.testing.assert_frame_equal(task15_sql, task15_pandas)

# %% [markdown]
# ### **16. SELECT planes.*, airlines.* FROM (SELECT DISTINCT carrier, tailnum FROM flights) AS cartail INNER JOIN planes ON cartail.tailnum=planes.tailnum INNER JOIN airlines ON cartail.carrier=airlines.carrier**

# %% [markdown]
# #### Query 16: INNER JOIN flights, planes, and airlines on carrier and tailnum and include only unique values from carrier and tailnum

# %%
task16_sql = pd.read_sql_query("""SELECT planes.*, airlines.* FROM
(SELECT DISTINCT carrier, tailnum FROM flights) AS cartail
INNER JOIN planes ON cartail.tailnum=planes.tailnum
INNER JOIN airlines ON cartail.carrier=airlines.carrier""",conn)

# %%
# Create subset with distinct carrier and tailnum
cartail = flights[['carrier', 'tailnum']].drop_duplicates()

# Join with planes and airlines
# Perform the inner join operations
task16_pandas = pd.merge(cartail, planes, on='tailnum', how='inner')
task16_pandas = pd.merge(task16_pandas, airlines, on='carrier', how='inner')
task16_pandas = task16_pandas[['tailnum', 'year', 'type', 'manufacturer', 'model', 'engines', 'seats',
       'speed', 'engine', 'carrier', 'name']]
task16_pandas

# %%
# Assuming task16_sql and task16_pandas are already defined
task16_sql_sorted = task16_sql.sort_values(by=['tailnum', 'carrier']).reset_index(drop=True)
task16_pandas_sorted = task16_pandas.sort_values(by=['tailnum', 'carrier']).reset_index(drop=True)

# %%
pd.testing.assert_frame_equal(task16_sql_sorted, task16_pandas_sorted)

# %% [markdown]
# ### **17. SELECT flights2.*, atemp, ahumid FROM (SELECT * FROM flights WHERE origin='EWR') AS flights2 LEFT JOIN (SELECT year, month, day, AVG(temp) AS atemp,AVG(humid) AS ahumid FROM weather WHERE origin='EWR' GROUP BY year, month, day ) AS weather2 ON flights2.year=weather2.year AND flights2.month=weather2.month AND flights2.day=weather2.day**

# %% [markdown]
# #### Query 17: LEFT JOIN flights and weather data with grouped daily averages where origin='EWR'

# %%
task17_sql = pd.read_sql_query("""SELECT flights2.*, atemp, ahumid
FROM (
SELECT * FROM flights WHERE origin='EWR'
) AS flights2 LEFT JOIN (
SELECT year, month, day, AVG(temp) AS atemp,
AVG(humid) AS ahumid
FROM weather
WHERE origin='EWR'
GROUP BY year, month, day ) AS weather2
ON flights2.year=weather2.year
AND flights2.month=weather2.month
AND flights2.day=weather2.day""", conn)

# %%
# Step 1: Filter flights table for origin='EWR'
flights_ewr = flights[flights['origin'] == 'EWR']

# Step 2: Aggregate weather data (calculate daily average temp and humidity)
weather_ewr = (
    weather[weather['origin'] == 'EWR']
    .groupby(['year', 'month', 'day'])[['temp', 'humid']]
    .mean()
    .reset_index()
    .rename(columns={'temp': 'atemp', 'humid': 'ahumid'})
)

# Step 3: LEFT JOIN flights_ewr with weather_ewr on year, month, day
task17_pandas = pd.merge(flights_ewr,weather_ewr, on=['year', 'month', 'day'], how='left')
task17_pandas

# %%
pd.testing.assert_frame_equal(task17_sql, task17_pandas)

# %% [markdown]
# ### **Conclusion:**  
#
# In this task, we successfully executed **17 SQL queries** on the `nycflights13` dataset and replicated them using **Pandas**. Our approach demonstrated the versatility of Pandas in handling **data extraction, filtering, aggregation, and joins** without relying on SQL.  
#
# Key takeaways:  
# - **Pandas vs SQL:** Pandas operations provided the same results as SQL but required a different approach for **grouping, ordering, and joins**.  
# - **Performance Considerations:** SQL is optimized for database queries, while Pandas is more flexible for in-memory computations. Future work could compare execution speeds.  
# - **Best Practices:** Proper indexing and optimized query structures improve performance in both SQL and Pandas.  
#
# This exercise provided valuable insights into using **SQL and Pandas interchangeably** for data wrangling, which is crucial for data analysts and engineers working with large datasets.
#

# %% [markdown]
# ### Optional Features(**)

# %%
import time

# Measure SQL execution time
start_sql = time.time()
task17_sql = pd.read_sql_query("""SELECT flights2.*, atemp, ahumid
FROM (
SELECT * FROM flights WHERE origin='EWR'
) AS flights2 LEFT JOIN (
SELECT year, month, day, AVG(temp) AS atemp,
AVG(humid) AS ahumid
FROM weather
WHERE origin='EWR'
GROUP BY year, month, day ) AS weather2
ON flights2.year=weather2.year
AND flights2.month=weather2.month
AND flights2.day=weather2.day""", conn)
end_sql = time.time()

# Measure Pandas execution time
start_pandas = time.time()
# Step 1: Filter flights table for origin='EWR'
flights_ewr = flights[flights['origin'] == 'EWR']

# Step 2: Aggregate weather data (calculate daily average temp and humidity)
weather_ewr = (
    weather[weather['origin'] == 'EWR']
    .groupby(['year', 'month', 'day'])[['temp', 'humid']]
    .mean()
    .reset_index()
    .rename(columns={'temp': 'atemp', 'humid': 'ahumid'})
)

# Step 3: LEFT JOIN flights_ewr with weather_ewr on year, month, day
task17_pandas = pd.merge(flights_ewr,weather_ewr, on=['year', 'month', 'day'], how='left')
task17_pandas
end_pandas = time.time()

print(f"SQL Execution Time: {end_sql - start_sql:.5f} seconds")
print(f"Pandas Execution Time: {end_pandas - start_pandas:.5f} seconds")