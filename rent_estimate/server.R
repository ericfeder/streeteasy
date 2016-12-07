library(shiny)

shinyServer(function(input, output) {
   
  output$estimate <- renderText({
    validate(need(!is.null(input$map_click), 
                  "Please choose a location on the map."))
    
    test_data <- data.frame(extra_bath = input$extra_bath,
                            elevator = input$elevator,
                            dishwasher = input$dishwasher,
                            laundry_in_unit = input$laundry_in_unit,
                            laundry_in_building = input$laundry_in_building,
                            ft = as.numeric(input$ft),
                            lat = as.numeric(input$map_click$lat),
                            lon = as.numeric(input$map_click$lng),
                            days_ago = 0,
                            beds = ordered(input$beds, levels = levels(model@data@env$input$beds)))
    estimate <- predict(model, test_data, OOB = TRUE)
    round(estimate)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -73.939, lat = 40.853, zoom = 15) %>%
      addTiles() 
  })
  
  observeEvent(input$map_click, {
    leafletProxy('map') %>%
      clearMarkers()%>%
      addMarkers(lng=input$map_click$lng, lat=input$map_click$lat)
  })
  
})
