#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(viridis)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DBI)
library(RSQLite)
library(dbplyr)
library(strings)

stns <- c("5005", "5006", "Cornwallis", "Lobster Ledge")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Annapolis County Water Quality Data"),
  
  verticalLayout(
    selectInput("station", "Station:", stns),
    uiOutput("timerangecontrol"),
    uiOutput("depthcontrol"),
    plotOutput("TempDOPlot")
  )
)

# Define server logic
server <- function(input, output) {
  
  con <- dbConnect(SQLite(), "strings.db")
  chosen_station_data <- reactive({
    tbl(con, "annapolis") %>% 
      filter(STATION == !!input$station) %>% 
      select(TIMESTAMP, DEPTH, VARIABLE, VALUE) %>% 
      collect() %>% 
      mutate(TIMESTAMP = as_datetime(TIMESTAMP))
    })
  

  output$depthcontrol <- renderUI({
    depths <- sort(unique(chosen_station_data()$DEPTH))
    pickerInput("depth", "Depth:", depths, 
                selected = depths, multiple = TRUE, 
                options = list(`actions-box` = TRUE))
  })
  
  output$timerangecontrol <- renderUI({
    min_date <- min(chosen_station_data()$TIMESTAMP, na.rm = TRUE)
    max_date <- max(chosen_station_data()$TIMESTAMP, na.rm = TRUE)
    sliderInput("timerange", "Time Range:",
                min = min_date,
                max = max_date,
                value = c(min_date, max_date),
                timeFormat = "%F %T")
  })
  
  # The data selected to be shown
  selected_data <- reactive({
    chosen_station_data() %>% 
      filter(DEPTH %in% input$depth) %>% 
      filter(TIMESTAMP >= input$timerange[1], TIMESTAMP <= input$timerange[2])
  }) 
  
  
  # Plot of data
  output$TempDOPlot <- renderPlot({
    ggplot_variables_at_depth(selected_data())
  })

}

# Run the application 
shinyApp(ui = ui, server = server, onStop(function() {dbDisconnect(con)}))
