library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Rent Estimates"),
  
  sidebarLayout(
    sidebarPanel(
      leafletOutput("map"),
      selectInput("beds", label = "Beds", choices = as.character(levels(model@data@env$input$beds))),
      numericInput("ft", label = "Size", min = 0, max = 2000, value = 500),
      checkboxInput("extra_bath", label = "More than 1 Bathroom"),
      checkboxInput("laundry_in_unit", label = "Washer/Dryer in Unit"),
      checkboxInput("laundry_in_building", label = "Laundry in Building"),
      checkboxInput("dishwasher", label = "Dishwasher"),
      checkboxInput("elevator", label = "Elevator")
    ),
    
    mainPanel(
       textOutput("estimate")
    )
  )
))
