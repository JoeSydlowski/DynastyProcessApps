library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    ggplot(x[x$pos == input$posFilter,], aes(dynpECR, redpECR)) + 
      geom_point(aes(name=mergename)) +
      geom_smooth(method='lm')
  }) 
  
})
