library(shiny)
library(curl)
library(shinythemes)
library(DT)



x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18,56,57)
x <- x[cols]


shinyServer(function(input, output) {
  
  dfA <- reactive({
    
    x$value <- round(10500*exp(x$dynoECR*input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideA)),]
    
  })
  
  dfB <- reactive({
    
    x$value <- round(10500*exp(x$dynoECR*input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideB)),]
    
  })
  
  
  output$tableA <- renderTable(
    dfA(),
    digits = 0
  )
  
  output$tableB <- renderTable(
    dfB(),
    digits = 0
  )
  
  output$textA <- renderText({ 
    if (sum(dfA()$value) > sum(dfB()$value)) {
      paste("Side A is winning the trade with", sum(dfA()$value), ".")
    } else if (sum(dfA()$value) < sum(dfB()$value)) {
      paste("Side B is winning the trade with", sum(dfB()$value), ".")
    }
  })  
  
  output$textB <- renderText({ 
    sum(dfB()$value)
  })  
  
  
  df <- reactive({
    
    x$value <- round(10500*exp(x$dynoECR*input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideA)) | (x$mergename %in% c(input$sideB)),]
    
  })
  
  output$results <- renderDT({
    datatable( df() )
  })
  
  
  
})
