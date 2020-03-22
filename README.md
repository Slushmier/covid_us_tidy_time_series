# covid_us_tidy_time_series
Provides a tidy time-series data set of Covid-19 case data collected by Johns Hopkins University.

I have noticed issues with some of the Covid-19 case datasets since the US data is at the city/county
level, or at the state level. This dataset aggregates all data to the state/district/territory level
(or for cruise ships where the passengers have not been repatriated yet).

Here's an attempt to provide a tidy, time-series csv of the data posted to Johns Hopkins page at
https://github.com/CSSEGISandData/COVID-19

There is also a Shiny app to view the data and forecasts for growth rates over the next 10 days: 
https://slushmier.shinyapps.io/covid_analysis/
