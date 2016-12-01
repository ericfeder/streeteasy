library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Rent Estimates"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("beds", label = "Beds", choices = as.character(levels(model@data@env$input$beds))),
      selectInput("extra_bath", label = "Baths", choices = c("1", "More than 1")),
      numericInput("ft", label = "Size", min = 0, max = 2000, value = 500),
      numericInput("lat", label = "Latitude", value = 40.8478),
      numericInput("lon", label = "Longitude", value = -73.9418)
    ),
    
    mainPanel(
       textOutput("estimate")
    )
  )
))
