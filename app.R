# Geoscience video library visualization
# January 21, 2022
# Katharine Sink

# load libraries
library(plyr)
library(tidyverse) # 
library(DT)
library(shiny)
library(shinydashboard)
library(leaflet)

# import dataset
library(datasets)
data(iris)
data(mtcars)
# save files in folder with app scripts

iris = data.frame(iris)
mtcars = data.frame(mtcars)

######################################

## DEFINE USER INTERFACE ##
# visualizations for the user (GUI)

ui = dashboardPage(
  
  # title for app
  dashboardHeader(title = "Ning Wang's App"), 
  
  
  # side panel with user inputs
  dashboardSidebar(
     
  # select dataset using pull down  
    selectizeInput(inputId = "dataset", 
                   label = "Choose a dataset", 
                   choices = c("Iris", "MtCars")), 
    
    # click button to verify change in dataset and update the displayed information
    actionButton("update", "Update view"),
    
    radioButtons(inputId = "input2", 
                 label = "Choose type", 
                 choices = c("Sega", "Nintendo"))
    
    
    
  ), # close dashboardSidebar
  
  # main outout for tables, maps
  dashboardBody(
    
    fluidRow(
      
      h4("Dataset location"),
        leafletOutput(outputId = "map")
      ), #fluidRow close 
      
    fluidRow(
   h4("Summary information"), 
    # summary statistics of selected dataset
    verbatimTextOutput(outputId = "summary")
    
    ), #fluidRow close 
    
    hr(), # horizontal rule
    
    fluidRow(
    # header
    h4("Data table"),
    # data table of selected dataset
      DTOutput(outputId = "datasetChoice")
   
     ) #fluidRow close
  
  ) # dashboardBody close
  
  
) # close dashboardPage




######################################

## SERVER ##
# server performs analyses and reactive outputs based on calls with render

server = function(input, output) {
  
  # location map
  output$map = renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenTopoMap")
  })
  
  
  
 datasetInput = eventReactive(input$update, {
   switch(input$dataset, 
          "Iris" = iris,  
          "MtCars" = mtcars)
 }, ignoreNULL = FALSE)
  
  
 output$summary = renderPrint({
   dataset = datasetInput()
   summary(dataset)
 })
 
 output$datasetChoice = renderDT({
   dataset = datasetInput()
 
 })
 
} # close server


shinyApp(ui = ui, server = server)