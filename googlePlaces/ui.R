#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# 

library(shiny)
library(httr)
library(glue)
library(jsonlite)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(igraph)
library(forcats)

base_trajet_total <- read.csv2('../data/base_trajet_total.csv')
stations <- unique(base_trajet_total$stop_name)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Where to Meet"),
  tags$h4("This shiny app allows you to find the best place to meet up with a friend."),
  tags$h4("Use the input on the left."),
  
  
  #We choose a sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      #1st station
      selectizeInput(inputId = "station1", label="Your departure point - metro station (type to avoid going through the list)", 
                     choices = stations, selected = "Bastille", multiple = FALSE, options = NULL),
      
      #2nd station
      selectizeInput(inputId = "station2", label="Your friends' departure point (type to avoid going through the list)",
                     choices = stations, multiple = FALSE, selected = "Bastille", options = NULL),
      
      #Slider
      sliderInput("radius",
                  "Radius around meeting point in metres:",
                  min = 10,
                  max = 500,
                  value = 200),
      #Selecter input
      selectInput(inputId = "type",
                  label = "Type of results: ",
                  selected = 3, 
                  choices = c('cafe', 'bar', 'restaurant'))
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Itinerary", 
                           textOutput("solution"),
                           leafletOutput("leaflet", width = "90%", height = "400px")),
                  tabPanel("User 1",leafletOutput("leaflet1", width = "90%", height = "400px")),
                  tabPanel("User 2",leafletOutput("leaflet2", width = "90%", height = "400px")),
                  tabPanel("Cool places around",
                           tags$h4("Here is a list of cool places around your meeting station"),
                           tags$h5("Use the inputs to specify the type of place you are looking for and its distance to the station."),
                           tableOutput("result_table"))
      )
    )
  ))
)
