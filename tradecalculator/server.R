library(shiny)
library(curl)
library(shinythemes)
library(DT)



x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18,56,57)
x <- x[cols]


shinyServer(function(input, output) {
  
  df <- reactive({
    
    x$value <- round(10500*exp(x$dynoECR*input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideA)) | (x$mergename %in% c(input$sideB)),]
    
  })

  output$results <- renderDT({
    datatable( df() )
  })
  
  
  
})
