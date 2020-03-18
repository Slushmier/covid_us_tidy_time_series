library(tidyverse)

reports <- list.files("csse_covid_19_data\\csse_covid_19_daily_reports",
                      pattern = ".csv")

varnames <- c()
for(report in reports){
  varname <- substr(report, 0, nchar(report) - 4)
  varname <- paste0("daily_", varname)
  varname <- str_replace_all(varname, "-", "_")
  varnames <- rbind(varnames, varname)
  report_run <- paste0("csse_covid_19_data\\csse_covid_19_daily_reports\\", report)
  exp <- paste0(varname, " <- read_csv(report_run)")
  eval(parse(text = exp))
  exp_2 <- paste0(varname, " <- dplyr::filter(", varname, ",`Country/Region` == 'US')")
  eval(parse(text = exp_2))
  print(report)
}

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
                  " , state_abbs, by = c('Province/State' = 'name'))")
  eval(parse(text = exp_3))
  exp_8 <- paste0(varnames[i], "$date <- substr('", as.character(varnames[i]),
                  "', 7, 16)")
  eval(parse(text = exp_8))
}

for(i in seq(11, 48, by = 1)){
  var_prov <- paste0(varnames[i], "$`Province/State`")
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

for(i in seq(49, nrow(varnames), by = 1)){
  #Standardize US Virgin Islands annotations
  var_prov <- paste0(varnames[i], "$`Province/State`")
  exp_4b <- paste0(var_prov, " <- ifelse(", var_prov,
                   " == 'Virgin Islands, U.S.', 'Virgin Islands', ", var_prov, ")")
  eval(parse(text = exp_4b))
  exp_3 <- paste0(varnames[i], " <- left_join(", varnames[i],
                  " , state_abbs, by = c('Province/State' = 'name'))")
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

for(i in seq(1, nrow(varnames), 1)){
  if(exists("covid_us_ts")){
    #Example: covid_us_ts <- rbind(covid_us_ts, daily_01_23_2020)
    exp_11 <- paste0("covid_us_ts <- rbind(covid_us_ts, ", varnames[i], ")")
    eval(parse(text = exp_11))
  }
  else{
    exp_12 <- paste0("covid_us_ts <- ", varnames[i])
    eval(parse(text = exp_12))
  }
}

covid_us_ts$date <- str_replace_all(covid_us_ts$date, "_", "/")

covid_us_ts<- covid_us_ts %>% group_by(state, date) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
            Recovered = sum(Recovered))

write_csv(covid_us_ts, "covid_us_time_series.csv")
