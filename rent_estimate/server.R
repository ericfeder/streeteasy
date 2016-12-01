library(shiny)

shinyServer(function(input, output) {
   
  output$estimate <- renderText({
    test_data <- data.frame(extra_bath = input$extra_bath == "More than 1",
                            ft = as.numeric(input$ft), 
                            lat = as.numeric(input$lat), 
                            lon = as.numeric(input$lon),
                            days_ago = 0,
                            beds = ordered(input$beds, levels = levels(model@data@env$input$beds)))
    estimate <- predict(model, test_data, OOB = TRUE)
    round(estimate)
  })
  
})
