library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)

queryurl <- "https://covidtracking.com/api/states/daily"

testing <- jsonlite::read_json(queryurl, simplifyVector = T)
testing[is.na(testing)] <- 0
testing$date <- as_date(as.character(testing$date), format = "%Y%m%d", tz = "UTC")
testing <- arrange(testing, state, date)

testing <- testing %>% group_by(state) %>% 
  select(-hospitalizedCumulative, -inIcuCumulative, -onVentilatorCumulative,
         -hash, -hospitalized) %>% 
  mutate_at(c("positive", "negative", "recovered", "death",
              "totalTestResults", "posNeg"),
            ~if_else(. < lag(., default = first(.)), lag(.), .))
  
write_csv(testing, "Data//testing_atlantic.csv")
