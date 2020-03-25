library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)
library(scales)

state_output <- function(input_state){
  
  filtered <- covid_ts %>% dplyr::filter(state == input_state)
  disp_date <- dplyr::filter(filtered, Type == 'Active', Number >= 1)
  disp_date <- disp_date[1, 2]
  
  filtered_gg <- ggplot(data = filtered) +
    geom_line(aes(x = date, y = Number, color = Type)) + xlab("Date")
  
  if (max(filtered$Number) > 10000){
    filtered_gg <- filtered_gg +
      scale_y_continuous(trans = 'log10', labels = comma) +
      ylab("Number - Logistic Scale")
  }
  
  filtered_gg + geom_forecast(data = dplyr::filter(filtered, Type == "Active"),
                              aes(x = date, y = Number), color = "red",
                              showgap = F) +
    ggtitle(paste0(input_state, " Covid Cases and Projections")) +
    theme(legend.title.align=0.5) +
    scale_x_date(date_breaks = "1 week",
                 labels = date_format("%d-%b"),
                 limits = (c(disp_date$date - 5, Sys.Date() + 10)))
}

covid_ts <- read_csv("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/covid_us_time_series.csv")
covid_ts$date <- as_date(covid_ts$date, format = "%m/%d/%Y", tz = "UTC")
covid_ts <- covid_ts %>% mutate(Active = Confirmed - Deaths - Recovered,
                                New = Active - lag(Active)) %>% 
  replace_na(list(Active = 0, New = 0)) %>% 
  dplyr::filter(!is.na(state)) %>% select(-Recovered)
covid_ts$New <- ifelse(covid_ts$New < 0, 0, covid_ts$New)
# us_all <- covid_ts %>% group_by(date) %>% 
#   summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths),
#                                  Recovered = sum(Recovered)) %>% 
#   mutate(Active = Confirmed - Deaths - Recovered, state = "US")
covid_ts <- covid_ts %>% gather(key = "Type", value = "Number", 
                                Confirmed, Deaths, New, Active)
# us_all <- us_all %>% gather(key = "Type", value = "Number",
#                             Confirmed, Deaths, Recovered, Active)

states <- covid_ts %>% dplyr::distinct(state) %>% arrange()
# covid_ts <- rbind(us_all, covid_ts)

ui <- fluidPage(
  titlePanel("Covid-19 Cases by State with 10-day Projections"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("stateinput",
              label = "State:",
              selected = "DC",
              choices = states),
      p("The red bands are ten day projections for numbers of Covid-19 cases.
        The narrow, dark red bands are 95% confidence intervals, the wider, 
        light red bands are 80% confidence intervals."),
      br(),
      p("Data comes from Johns Hopkins University COVID-19 Github page: 
        https://github.com/CSSEGISandData/COVID-19"),
      br(),
      p("Projections are done with the forecast package in R."),
      br(),
      p("The 'recovered' cases have been aggregated into US totals, so are not
        currently accurate at the state level."),
      br(),
      p("Note: I am definitely not an epidemiologist; ask one for more 
        information on Covid-19.")
      ),
  
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
server <- function(input, output){
  output$plot <- renderPlot({
    state_output(input$stateinput)
  })
}

shinyApp(ui, server)