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
library(sensorstrings)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


ui <- fluidPage(

    # Application title
    titlePanel("Pictou County Water Quality Data"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "stn",
                        label = "Station:",
                        choices = c("Back Harbour", "Inner Harbour", 
                                    "Melmerby Beach", "Olding Island", 
                                    "Piper Lake", "Robinson Cove", 
                                    "Waterside" )),
            uiOutput("depth_options"),
            uiOutput("time_control")
            
        ),
        mainPanel(
           plotOutput("varPlot")
        )
    )
)


server <- function(input, output) {

  data <- read_csv("Pictou_County_Water_Quality_Data.csv") %>% 
    as_tibble() %>% 
    ss_reformat_old_data() %>% 
    mutate(timestamp_utc = as_datetime(timestamp_utc))
  
  chosen_station_data <- reactive({filter(data, station == input$stn)
    })
  
  output$depth_options <- renderUI({
    depths <- sort(unique(chosen_station_data()$sensor_depth_at_low_tide_m))
    pickerInput("depth", "Depth:", depths,
                selected = depths, multiple = TRUE,
                options = list(`actions-box` = TRUE))
    })
  
  output$time_control <- renderUI({
    min_date <- min(chosen_station_data()$timestamp_utc, na.rm = TRUE)
    max_date <- max(chosen_station_data()$timestamp_utc, na.rm = TRUE)
    sliderInput("timerange", "Time Range:",
                min = min_date,
                max = max_date,
                value = c(min_date, max_date),
                timeFormat = "%F %T")
  })
  
  chosen_data <- reactive({
    chosen_station_data() %>%
      filter(sensor_depth_at_low_tide_m %in% input$depth) %>%
      filter(timestamp_utc >= input$timerange[1], timestamp_utc <= input$timerange[2])
  })
  
  output$varPlot <- renderPlot({
    ss_ggplot_variables(chosen_data(), superchill = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
