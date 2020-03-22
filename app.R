library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)

state_output <- function(input_state){
  filtered <- covid_ts %>% dplyr::filter(state == input_state, date >= "2020-03-01")
  #filtered$date <- as_date(filtered$date, format = "%m/%d/%Y", tz = "UTC")
  #filtered <- filtered %>%  mutate(Active = Confirmed - Deaths - Recovered)
  #filtered <- gather(filtered, key = "Type", value = "Number",
  #                   Confirmed, Deaths, Recovered, Active)
  #
  # There are some results that do not make sense
  # filtered_ts <- dplyr::filter(filtered, Type == 'Active') %>% 
  #   select(date, Number) %>% ts()
  filtered_gg <- ggplot(data = filtered) +
    geom_line(aes(x = date, y = Number, color = Type))
  filtered_gg + geom_forecast(data = dplyr::filter(filtered, Type == "Active"),
                              aes(x = date, y = Number), color = "red",
                              showgap = F) +
    ggtitle(paste0(input_state, " Covid Cases and Projections")) +
    theme(legend.title.align=0.5) + xlab("Date")
}

covid_ts <- read_csv("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/covid_us_time_series.csv")
covid_ts$date <- as_date(covid_ts$date, format = "%m/%d/%Y", tz = "UTC")
covid_ts <- covid_ts %>% mutate(Active = Confirmed - Deaths - Recovered)
covid_ts <- covid_ts %>% gather(key = "Type", value = "Number", 
                                Confirmed, Deaths, Recovered, Active)

states <- covid_ts %>% dplyr::distinct(state) %>% arrange()

ui <- fluidPage(
  titlePanel("Covid-19 cases by state and 10-day projections"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("stateinput",
              label = "State:",
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