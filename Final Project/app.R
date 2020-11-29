# CAMELS dataset, common gauges (15) with MOPEX
# Katharine Sink 11/4/20
# Shiny app for EDA

# Load libraries
library(shiny)
library(shinythemes)
library(shinydashboard) # UI
library(lubridate) # Date manipulation
library(DT) # Makes datatables interactive in UI
library(ggplot2) # Create plots
library(dplyr) # Data manipulating (part of tidyverse)
library(leaflet) # Mapping
library(tidyr) # Reshaping data (part of tidyverse)

##################################################
# DATA IMPORT AND ORGANIZATION FOR APP

# Read in datatable/tibble that was saved and exported as RDS 
# from Camels15gauges.R script
# Modify table by removing columns SWE, RAIM, MOD_RUN
# and move date column from the last row to second row

shinydata = readRDS("shinydata.rds")
shinydata2 = shinydata[-c(5,7,11)]
shinydata2 = shinydata2 %>%  relocate(DATE, .before = "YR")

# shinydata2 with 10 variables and 184,171 observations
# Resultant column number and header   
# 1 - GaugeID (8 digit USGS gauge number, character)
# 2 - DATE (combined YR, MNTH, DY lubridate, date)
# 3 - YR (4 digit year, 1981 - 2014, numeric)
# 4 - MNTH (1 digit month, 1 - 12, numeric)
# 5 - DY (numeric)
# 6 - PRCP (precipitation (PRCP) in mm/day)
# 7 - TAIR (mean daily air temp (TAIR) in celcius)
# 8 - PET (potential evapotranspiration (PET) in mm/day)
# 9 - ET (evapotranspiration (ET) in mm/day from SAC model)
# 10 - OBS_RUN (observed runoff (OBS_RUN) in mm/day from USGS)


# Variable names correspond to column headers from shinydata2 
# PRCP, TAIR, PET, ET, OB_RUN are columns 6 through 10
# respectively, and the data is all numeric

# Variable names and named vector to correspond to header
# name in datatable
varNames = c("Precipitation" = "PRCP", 
             "Air Temperature" = "TAIR",
             "Potential ET" = "PET", 
             "Actual ET" = "ET", 
             "Runoff" = "OBS_RUN")

# Years are from 1981 - 2014
# Column 3 in shinydata2, numeric
years = unique(shinydata2$YR)

# To later add name of months for refined filters, use "month.name"

# 8 digit USGS gauge number, 15 total gauges
# Column 1 in shinydata2, character
gaugeIds = unique(shinydata2$GaugeID)

# Gauge names and corresponding Gauge ID values as a named vector
gaugeNames = c("Turkey Creek near Seneca (06814000)" = "06814000",
               "Soldier Creek near Delia (06892000)" = "06892000",
               "Marais Des Cygnes River near Reading (06910800)" = "06910800",
               "Dragoon Creek near Burlingame (06911900)" = "06911900",
               "Chikaskia River near Corbin (07151500)" = "07151500",
               "Cedar Creek near Cedar Point (07180500)" = "07180500",
               "Timber Creek near Collinsville (08050800)" = "08050800",
               "North Fork Guadalupe River near Kyle (08171300)" = "08171300",
               "Blanco River near Kyle (08189500)" = "08189500",
               "Mission River at Refugio (08189500)" = "08189500",
               "East Fork White River near Fort Apache (09492400)" = "09492400",
               "White River near Fort Apache (09494000)" = "09494000",
               "Cibecue Creek near Chysotile (09497800)" = "09497800",
               "Cherry Creek near Globe (09497980)" = "09497980",
               "Los Gatos Creek near Coalinga (11224500)" = "11224500")


# USGS gauge latitudes
gaugeLat = as.numeric(c(39.94778, 39.23833, 38.56701, 38.71069, 37.12891,
                        38.19645, 33.55455, 30.0641, 29.97938, 28.29195, 
                        33.82227, 33.73644, 33.84311, 33.82783, 36.21468))

# USGS gauge longitudes
gaugeLong = as.numeric(c(-96.10862, -95.8886, -95.96163, -95.83603, -97.60144, 
                         -96.82458, -96.94723, -99.38699, -97.91, -97.27916, 
                         -109.81454, -110.16677, -110.55761, -110.85623, -120.47071))

# Combine USGS gauge IDs, latitude and longitude into one datatable
gaugeLatLong = tibble(x = gaugeIds, y = gaugeLat, z = gaugeLong)

############################################
# Define user interface (UI)

ui = dashboardPage(
  
  
  
  dashboardHeader(title = "CAMELS EDA"), 
                  
  
# Side panel with user inputs  
  dashboardSidebar(
    
# Select USGS gauge, gaugeNames corresponds to Gauge ID in shinydata2
# based on assigned named vector     
    selectizeInput(inputId = "gauge1", 
                   label = "Choose USGS Stream Gauge",
                   choices = gaugeNames),
    
# Select variable (PRCP, TAIR, PET, ET, OBS_RUN)    
    radioButtons(inputId = "variable1", 
                 label = "Choose variable",
                 choices = varNames),
    
# Select start year and end year for analysis
# which allows a smaller window of time to be viewed
    sliderInput(inputId = "yrRange1",
                label = "Select the range of years:",
                min = 1981, max = 2014, sep = "",
                value = c(1981, 2000)),
    
# Select annual or monthly aggregation for stats and plots
# Annual is the mean and sum for each year, monthly is the 
# mean and sum for the corresponding month over the entire selected
# time span
    radioButtons(inputId = "temporal1",
                 label = "Temporal aggregation:",
                 choices = c("Annual" = "YR", "Monthly" = "MNTH"))
  ),
  
# Main output for statistics, map, and plots
  dashboardBody(

# Top row with two boxes   
    fluidRow(

# Map location for selected gauge, using lat/long   
      box(title = "Stream Gauge Location", 
          solidHeader = TRUE,
            leafletOutput("map"), width = 7),
      
# Basic summary statistics for the selected gauge, variable over range
# Will expand to include more statistical analyses
      box(title = "Summary Statistics", 
          solidHeader = TRUE, 
          verbatimTextOutput("statsTable"),
          width = 5)
      
      
     
    ),

# Bottom row for plots displays
    fluidRow(
      
# Histogram for selected gauge, variable, and aggregation of time span      
      box(title = "Histogram", status = "primary",
          solidHeader = TRUE,
          plotOutput("histPlot"), width = 4),
     
# Box plot for selected gauge, variable, and aggregation of time span           
       box(title = "Box Plot", status = "primary",
          solidHeader = TRUE, 
          plotOutput("boxPlot"),
          width = 4),
     
# Line plot (time series) for selected gauge, variable, and aggregation of time span          
       box(title = "Time Series Plot", status = "primary",
          solidHeader = TRUE, 
          plotOutput("timePlot"), width = 4)
    )
  )
)  

###############################################
# Server performs analyses and reactive outputs based on calls
# with render

server = function(input, output) {

  
# Create reactive datatable (shiny_data) based on shinydata2
# Table will update and filter based on user inputs
  shiny_data <- reactive({
    shinydata2 %>% 
      group_by(GaugeID, YR, MNTH) %>%  #arrange in groups in table
      filter(GaugeID == input$gauge1,    # filtered for selected gauge
             YR >= input$yrRange1[1],
             YR <= input$yrRange1[2]) %>%   # filtered for selected range
      select(YR, MNTH, input$variable1)    # filter for selected variable
  })

# Output of summary statistics, text since it just prints    
  output$statsTable = renderPrint({
   summary(shiny_data()[[input$variable1]])
  })
 
# Make reactive variable   
  gaugeLoc <- reactive({ 
    gaugeLatLong %>% 
      filter(x == input$gauge1)
  })    
 
# Location map    
  output$map = renderLeaflet({
    leaflet(data = gaugeLoc()) %>% 
      addProviderTiles("OpenTopoMap") %>% 
      addCircleMarkers(lng = ~z, lat = ~y, popup = ~x) 
  })

# New reactive expression which all plots will use for data
# Based on the reactive datatable (shiny_data)
  plot_data <- reactive({
    shiny_data() %>% 
      group_by(.data[[input$temporal1]]) %>%   # group by YR or MNTH
      summarise(total = sum(.data[[input$variable1]]), # overall total for variable
                mean = mean(.data[[input$variable1]])) # overall average 
  })

  # How to switch to mean??
# Histogram plot for annual or monthly output (based on temporal selection)    
  output$histPlot = renderPlot({
    ggplot(data = plot_data(), aes(x = total)) +
      geom_histogram(binwidth = 150, color = "black", fill = "#6699FF") +
      ylab("Frequency") +
      xlab(input$variable1) +
      theme_minimal()
  })
  
  output$timePlot = renderPlot({
    ggplot(data = plot_data(), aes(x = .data[[input$temporal1]], y = total)) +
      geom_line(color = "6600CC", linetype = 5, size = 1.5) +
      theme_minimal()
  })
  
  output$boxPlot = renderPlot({
    ggplot(data = plot_data(), aes(x = .data[[input$temporal1]], y = total)) +
      geom_boxplot(width = 0.5) +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)

