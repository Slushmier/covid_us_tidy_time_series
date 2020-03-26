library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)
library(scales)

state_output <- function(input_state){
  if(input_state == "All US") {
    filtered <- covid_us %>% dplyr::filter(Type != "Recovered")
  } else {
    filtered <- covid_ts %>% dplyr::filter(state == input_state)
    }
  
  disp_date <- dplyr::filter(filtered, Type == 'Active', Number >= 1)
  if(input_state == "All US"){
    disp_date <- disp_date[1, 1]
  } else{
      disp_date <- disp_date[1, 2]
      }
  
  filtered_gg <- ggplot(data = filtered) + xlab("Date")  
    
  if (max(filtered$Number) > 20000){
    filtered_gg <- filtered_gg + 
      geom_smooth(aes(x = date, y = Number, color = Type),
                  se = F, size = 0.75) +
      geom_forecast(data = dplyr::filter(filtered, Type == "Active"), 
                    aes(x = date, y = Number),
                    color = "red", showgap = F, size = 0.75) +
      scale_y_continuous(trans = 'log10', labels = comma) +
      ylab("Number - Logistic Scale")
  } else {
    filtered_gg <- filtered_gg + 
      geom_line(aes(x = date, y = Number, color = Type), size = 0.75) +
      geom_forecast(data = dplyr::filter(filtered, Type == "Active"),
                    aes(x = date, y = Number),
                    color = "red", showgap = F, size = 0.75)
  }
  
  filtered_gg +
    ggtitle(paste0(input_state, " Covid Cases and Projections")) +
    theme(legend.title.align=0.5) +
    scale_x_date(date_breaks = "1 week",
                 labels = date_format("%d-%b"),
                 limits = (c(disp_date$date - 5, Sys.Date() + 10)))
}

covid_ts <- read_csv("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/covid_us_time_series.csv")
covid_ts$date <- as_date(covid_ts$date, format = "%m/%d/%Y", tz = "UTC")
covid_ts <- covid_ts %>% gather(key = "Type", value = "Number", 
                                Confirmed, Deaths, New, Active)

covid_us <- read_csv("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/covid_us_time_series_aggregate.csv") %>% 
  gather(key = "Type", value = "Number",
         Confirmed, Deaths, Recovered, Active, New)
covid_us$date <- as_date(covid_us$date, format = "%m/%d/%Y", tz = "UTC")

states <- covid_ts %>% dplyr::distinct(state) %>% arrange()
states <- rbind("All US", states)

ui <- fluidPage(
  titlePanel("Covid-19 Cases by State with 10-day Projections"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("stateinput",
              label = "State:",
              selected = "All US",
              choices = states),
        p("The red bands are ten day projections for numbers of Covid-19 cases.
          The narrow, dark red bands are 95% confidence intervals, the wider, 
          light red bands are 80% confidence intervals."),
        br(),
        p("Data comes from",
        tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
        "the Johns Hopkins University COVID-19 Github page.")),
        br(),
        p("Projections are done with the forecast package in R."),
        br(),
        p("Note: I am definitely not an epidemiologist; ask one for more 
        information on Covid-19.",  
        tags$a(href = "https://github.com/Slushmier/covid_us_tidy_time_series", 
                    "Here is the GitHub repository for this page."))
      ),
  
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Data", tableOutput("table"))
        )
      )
    )
  )
  
server <- function(input, output){
  output$plot <- renderPlot({
    state_output(input$stateinput)
  })
  output$table <- renderTable({
    if(input$stateinput == "All US") {
      dataout <- covid_us %>% 
        spread(key = Type, value = Number) %>% 
        arrange(desc(date))
      dataout$date <- as.character(dataout$date)
    } else {
      dataout <- covid_ts %>%
        dplyr::filter(state == input$stateinput) %>% 
        spread(key = Type, value = Number) %>% 
        arrange(desc(date))
      dataout$date <- as.character(dataout$date)
    }
    dataout}, digits = 0
  )
}

shinyApp(ui, server)