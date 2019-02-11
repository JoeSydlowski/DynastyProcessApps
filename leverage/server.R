library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18,56:62)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    ggplot(x[x$pos == input$posFilter,], aes(dynpECR, redpECR)) + 
      geom_point(aes(name=mergename)) +
      geom_smooth(method='lm')
  }) 
  
})
