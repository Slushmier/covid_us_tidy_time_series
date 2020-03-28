# covid_us_tidy_time_series
This repository is intended to provide a tidy time-series data set of Covid-19 case data collected by 
Johns Hopkins University (JHU) at https://github.com/CSSEGISandData/COVID-19.

I have also created a Shiny app to view the data and forecasts for state growth rates over the next 10 days: 
https://slushmier.shinyapps.io/covid_analysis/

## Overview of files in dataset
### Files in main folder

**app.R** - code for the Shiny app I have running at https://slushmier.shinyapps.io/covid_analysis/  
**covid_pull.R** - formats the daily reports from the JHU datasets to create a clean tidy dataset by US districts, states,
and territories  
**covid_us_time_series.csv** - The most recently updated tidy, time-series dataset of the JHU data. I upload this either every
day after the District of Columbia announces new cases or early the following morning. Contains colums for state, date, Confirmed,
Deaths, Recovered, and New Cases  
**covid_us_time_series_aggregate.csv** - Same as above; has aggregate counts for whole country  
**covid_us_time_series_counties.csv** - Same as two above with county counts starting from 23 March 2020.  
**simplify_spatial.R** - simplifies a TIGR counties shapefile and merges it with population counts; probably only works on my computer  
**testing_data.R** - Pulls and writes a CSV of the dataset compiled at https://covidtracking.com/api/states/daily to *data/testing_atlantic.csv*   

### Files in data folder  

**counties_simple shapefile** - Provides a US counties shapefile with simplified boundaries  
**county_pop.csv** - 2018 Census data of population count by county  
_**state_all.geojson**_ - Couples testing data and state boundaries, used for map in **app.R**  
**state_pop.csv** - 2018 Census data of population count by state  
**states_simple shapefile** - Provides a US states shapefile with simplified boundaries  
**testing_atlantic.csv** - CSV of data on Covid-19 tests provided by state, compiled from covidtracking.com  
