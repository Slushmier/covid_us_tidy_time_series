library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)
library(scales)
library(sf)
library(leaflet)

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

state_all <- sf::st_read("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/Data/state_all.geojson")
# Popup for US map, will move
state_popup <- paste0("<strong>Covid-19 Data by State</strong>",
                      "<br><br><strong>State: </strong>", 
                      state_all$stat_nm, 
                      "<br><strong>Date of Data: </strong>", 
                      state_all$date,
                      "<br><strong>2018 Population Estimate (census):
                      </strong>",
                      state_all$POPESTI,
                      "<br><strong>Confirmed cases (JHU): </strong>",
                      state_all$Confirmed,
                      "<br><strong>Deaths (JHU): </strong>",
                      state_all$Deaths,
                      "<br><strong>New cases (JHU): </strong>",
                      state_all$New,
                      "<br><strong>Postive tests (covidtracking.com): </strong>",
                      state_all$positive,
                      "<br><strong>Negative tests: </strong>",
                      state_all$negative,
                      "<br><strong>Total test results: </strong>",
                      state_all$totalTestResults,
                      "<br><strong>Positive test rate: </strong>",
                      round(state_all$pos_test_rate, 5),
                      "<br><strong>Tests per 1000 people: </strong>",
                      round(state_all$test_per_cap, 5),
                      "<br><strong>Population density (per kmsq): </strong>",
                      round(state_all$pop_density, 2),
                      "<br><strong>Cases per 1000 people: </strong>",
                      round(state_all$case_rate, 5))

county_all <- sf::st_read("https://raw.githubusercontent.com/Slushmier/covid_us_tidy_time_series/master/Data/counties_all.geojson")
county_popup <- paste0("<strong>Covid-19 Data by County</strong>",
                       "<br><br><strong>State: </strong>", 
                       county_all$Province_State, 
                       "<br><br><strong>County: </strong>", 
                       county_all$NAMELSA, 
                       "<br><strong>Date of Data: </strong>", 
                       county_all$date,
                       "<br><strong>2018 Population Estimate (census):
                      </strong>",
                       county_all$POPESTI,
                       "<br><strong>Confirmed cases (JHU): </strong>",
                       county_all$Confirmed,
                       "<br><strong>Deaths (JHU): </strong>",
                       county_all$Deaths,
                       "<br><strong>Population density (per kmsq): </strong>",
                       round(county_all$pop_density, 2),
                       "<br><strong>Cases per 1000 people: </strong>",
                       round(county_all$case_rate, 5))

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
                    tabPanel("Case Projections", plotOutput("plot")),
                    tabPanel("US Testing", leafletOutput("usmap")),
                    tabPanel("County Map", leafletOutput("countymap")),
                    tabPanel("Projection Data", tableOutput("table"))
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
  output$usmap <- renderLeaflet({
    pal_map <- colorQuantile("Blues", domain = state_all$test_per_cap, n = 4)
    
    leaflet(state_all) %>% 
      addTiles() %>% 
      addPolygons(color = "gray", weight = 1, smoothFactor = 0.5,
                  opacity = 0.5, fillOpacity = 0.2, 
                  fillColor = ~colorQuantile("Blues", test_per_cap, n = 4)
                  (test_per_cap),
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = T),
                  popup = state_popup,
                  label = ~paste0(stat_nm, ": ", Confirmed, " confirmed cases."),
                  labelOptions = labelOptions(direction = "auto")) %>% 
      addLegend("topright", pal = pal_map, values = ~test_per_cap,
                title = "Tests Per 1000 People",
                opacity = 0.5,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  p = paste0(round(p * 100), '%')
                  cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))
                  # mouse over the legend labels to see the percentile ranges
                  paste0(
                    '<span title="', p[-n], " - ", p[-1], '">', cuts,
                    '</span>')
                }
      ) %>% 
      setView(-103.771556, 44.967243, zoom = 3) %>% 
      addFullscreenControl()
  })
  output$countymap <- renderLeaflet({
    pal_map <- colorNumeric("Reds", domain = county_all$case_rate)
    
    leaflet(county_all) %>% 
      addTiles() %>% 
      addPolygons(color = "gray", weight = 0.1, smoothFactor = 0.5,
                  opacity = 0.8, fillOpacity = 0.7, 
                  fillColor = ~colorNumeric("Reds", log(case_rate+1))
                  (log(case_rate+1)),
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = T),
                  popup = county_popup,
                  label = ~paste0(NAMELSA, ": ", Confirmed, " confirmed cases."),
                  labelOptions = labelOptions(direction = "auto")) %>% 
      addLegend("topright", pal = pal_map, values = ~case_rate,
                title = "Confirmed Cases<br>Per 1000 People<br>",
                opacity = 1/exp(1)) %>% 
      setView(-103.771556, 44.967243, zoom = 3) %>% 
      addFullscreenControl()
  })
}

shinyApp(ui, server)