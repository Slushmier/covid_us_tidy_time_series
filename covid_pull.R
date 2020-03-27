library(tidyverse)

reports <- list.files("csse_covid_19_data\\csse_covid_19_daily_reports",
                      pattern = ".csv")

varnames <- c()

### Variable names change with 3/23 daily report
for(report in reports){
  varname <- substr(report, 0, nchar(report) - 4)
  varname <- paste0("daily_", varname)
  varname <- str_replace_all(varname, "-", "_")
  varnames <- rbind(varnames, varname)
  report_run <- paste0("csse_covid_19_data\\csse_covid_19_daily_reports\\", report)
  exp <- paste0(varname, " <- read_csv(report_run)")
  eval(parse(text = exp))
  exp_2b <- paste0(varname, " <- ", varname,
                   " %>% purrr::set_names(~str_replace_all(., '/', '_'))")
  eval(parse(text = exp_2b))
  exp_2 <- paste0(varname, " <- dplyr::filter(", varname, ",`Country_Region` == 'US')")
  eval(parse(text = exp_2))
  # Example: daily_03_22_2020 <- daily_03_22_2020 %>% 
  #            purrr::set_names(~str_replace_all(., "/", "_"))

  print(report)
}

### Accounts for format change from 3/23 daily report forward

state_abbs <- data.frame(name = state.name, state = state.abb)
state_abbs$name <- as.character(state_abbs$name)
state_abbs$state <- as.character(state_abbs$state)
state_abbs <- rbind(state_abbs, c("District of Columbia", "DC"),
                    c("Diamond Princess", "Cruise"),
                    c("Grand Princess", "Cruise"),
                    c("Puerto Rico", "PR"),
                    c("Guam", "GU"),
                    c("Virgin Islands", "VI"),
                    c("Chicago", "IL"))

for(i in seq(1, 10, by = 1)){
  exp_3 <- paste0(varnames[i], " <- left_join(", varnames[i],
                  " , state_abbs, by = c('Province_State' = 'name'))")
  eval(parse(text = exp_3))
  exp_8 <- paste0(varnames[i], "$date <- substr('", as.character(varnames[i]),
                  "', 7, 16)")
  eval(parse(text = exp_8))
}

for(i in seq(11, 48, by = 1)){
  var_prov <- paste0(varnames[i], "$`Province_State`")

  exp_4 <- paste0(var_prov, " <- ifelse(", var_prov,
                  " == 'Washington, D.C.', 'Washington, DC', ", var_prov, ")")
  eval(parse(text = exp_4))

  exp_5 <- paste0(varnames[i], "$state <- substr(str_extract(",
                  var_prov, ", regex('[,][:space:][:UPPER:]{2}')), 3, 4)")
  eval(parse(text = exp_5))
  # Example: daily_03_09_2020$state <- replace_na(daily_03_09_2020$state, "Cruise")
  exp_6 <- paste0(varnames[i], "$state <- replace_na(", varnames[i],
                  "$state, 'Cruise')")
  eval(parse(text = exp_6))
  
  # Example: daily_03_09_2020 <- dplyr::group_by(daily_03_09_2020, state) %>% 
  #   summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths),
  #             Recovered = sum(Recovered))
  exp_7 <- paste0(varnames[i], " <- dplyr::group_by(", varnames[i],
                  ", state) %>% summarize(Confirmed = sum(Confirmed), 
                  Deaths = sum(Deaths), Recovered = sum(Recovered))")
  eval(parse(text = exp_7))
  
  # Example: daily_03_09_2020$date <- substr("daily_03_09_2020", 7, 16)
  exp_8 <- paste0(varnames[i], "$date <- substr('", as.character(varnames[i]),
                  "', 7, 16)")
  eval(parse(text = exp_8))
  }

for(i in seq(49, 61, by = 1)){
  #Standardize US Virgin Islands annotations
  var_prov <- paste0(varnames[i], "$`Province_State`")
  exp_4b <- paste0(var_prov, " <- ifelse(", var_prov,
                   " == 'Virgin Islands, U.S.', 'Virgin Islands', ", var_prov, ")")
  eval(parse(text = exp_4b))
  exp_3 <- paste0(varnames[i], " <- left_join(", varnames[i],
                  " , state_abbs, by = c('Province_State' = 'name'))")
  eval(parse(text = exp_3))
  exp_8 <- paste0(varnames[i], "$date <- substr('", as.character(varnames[i]),
                  "', 7, 16)")
  eval(parse(text = exp_8))
}

var_counties <- c()
for(i in seq(62, length(varnames), by = 1)){
  #Standardize US Virgin Islands annotations
  var_prov <- paste0(varnames[i], "$`Province_State`")
  exp_4b <- paste0(var_prov, " <- ifelse(", var_prov,
                   " == 'Virgin Islands, U.S.', 'Virgin Islands', ", var_prov, ")")
  eval(parse(text = exp_4b))
  
  # Copying county numbers to separate data frames for later export
  var_county <- paste0(varnames[i], "_county")
  exp_4c <- paste0(var_county, " <- ", varnames[i])
  eval(parse(text = exp_4c))
  var_counties <- rbind(var_counties, var_county)
  
  exp_3 <- paste0(varnames[i], " <- left_join(", varnames[i],
                  " , state_abbs, by = c('Province_State' = 'name'))")
  eval(parse(text = exp_3))
  exp_8 <- paste0(varnames[i], "$date <- substr('", as.character(varnames[i]),
                  "', 7, 16)")
  eval(parse(text = exp_8))
}

for(i in seq(1, nrow(varnames), by = 1)){
  #Example: dplyr::select(daily_01_24_2020, Confirmed, Deaths, Recovered, state, date)
  exp_9 <- paste0(varnames[i], " <- dplyr::select(", varnames[i],
                  ", Confirmed, Deaths, Recovered, state, date)")
  eval(parse(text = exp_9))
  #Example: daily_01_22_2020 <- daily_01_22_2020 %>%
  # complete(state = state_abbs$state,
  #          fill = list(Confirmed = 0, Deaths = 0, Recovered = 0))
  exp_10 <- paste0(varnames[i], " <- ", varnames[i], " %>% complete(
                   state = state_abbs$state, fill = list(Confirmed = 0,
                   Deaths = 0, Recovered = 0, date = substr('", as.character(varnames[i]),
                   "', 7, 16)))")
  eval(parse(text = exp_10))
}

# Create state time-series
for(i in seq(1, nrow(varnames), 1)){
  if(exists("covid_us_ts")){
    #Example: covid_us_ts <- rbind(covid_us_ts, daily_01_23_2020)
    exp_11 <- paste0("covid_us_ts <- rbind(covid_us_ts, ",
                     varnames[i], ")")
    eval(parse(text = exp_11))
  }
  else{
    exp_12 <- paste0("covid_us_ts <- ", varnames[i])
    eval(parse(text = exp_12))
  }
}

### Counties
for(i in seq(1, nrow(var_counties), 1)){
  if(exists("covid_us_ts_counties")){
    #Example: covid_us_ts_counties <- rbind(covid_us_ts_counties, daily_01_23_2020_county)
    exp_11 <- paste0("covid_us_ts_counties <- rbind(covid_us_ts_counties, ", var_counties[i], ")")
    eval(parse(text = exp_11))
  }else{
    exp_12 <- paste0("covid_us_ts_counties <- ", var_counties[i])
    eval(parse(text = exp_12))
  }
}

covid_us_ts_counties <- covid_us_ts_counties %>%
  dplyr::filter(!is.na(FIPS))

covid_us_ts$date <- str_replace_all(covid_us_ts$date, "_", "/")

covid_us_ts<- covid_us_ts %>% group_by(state, date) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
            Recovered = sum(Recovered)) 

covid_us <- covid_us_ts %>% group_by(date) %>% 
  summarize( Confirmed = sum(Confirmed), Deaths = sum(Deaths),
             Recovered = sum(Recovered)) %>% 
  mutate(Active = Confirmed - Deaths - Recovered,
         New = Confirmed - lag(Confirmed)) %>%
  replace_na(list(New = 1))

covid_us_ts <- covid_us_ts %>% mutate(Active = Confirmed - Deaths - Recovered,
                      New = Confirmed - lag(Confirmed)) %>% 
  replace_na(list(Active = 0, New = 0)) %>% 
  dplyr::filter(!is.na(state)) %>% select(-Recovered)

covid_us$New <- ifelse(covid_us$New < 0, 0, covid_us$New)
covid_us$Recovered <- ifelse(covid_us$Recovered < lag(covid_us$Recovered),
                             lag(covid_us$Recovered), covid_us$Recovered)
covid_us <- covid_us %>% replace_na(list(Recovered = 0))
covid_us_ts$New <- ifelse(covid_us_ts$New < 0, 0, covid_us_ts$New)

write_csv(covid_us_ts_counties, "covid_us_time_series_counties.csv")
write_csv(covid_us_ts, "covid_us_time_series.csv")
write_csv(covid_us, "covid_us_time_series_aggregate.csv")