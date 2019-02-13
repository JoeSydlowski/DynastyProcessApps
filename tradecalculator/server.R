library(shiny)
library(curl)
library(shinythemes)
library(DT)



x <-
  read.csv(
    curl(
      "https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"
    )
  )
cols <- c(15:18, 56, 57)
x <- x[cols]


shinyServer(function(input, output) {
  dfA <- reactive({
    x$value <- round(10500 * exp(x[input$numQB] * input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideA)), ]
    
  })
  
  dfB <- reactive({
    x$value <- round(10500 * exp(x[input$numQB] * input$slider1))
    
    x <- x[(x$mergename %in% c(input$sideB)), ]
    
  })
  
  output$tableA <- renderTable({dfA()
                               #digits = 0
    })
  
  output$tableB <- renderTable({dfB()
                               #digits = 0
    })
  
  sumdfA <- reactive({
    sum(dfA()$value)
  })
  
  sumdfB <- reactive({
    sum(dfB()$value)
  })
  
  output$winner <- renderText({
    if (nrow(dfA()) > 0 & nrow(dfB()) > 0) {
      if (sumdfA() > sumdfB()) {
        paste("Side A is winning the trade by",
              sum(dfA()$value) - sum(dfB()$value) ,
              ".")
      } else if (sum(dfA()$value) < sum(dfB()$value)) {
        paste("Side B is winning the trade by",
              sum(dfB()$value) - sum(dfA()$value) ,
              ".")
      }
    }
  })
  
  output$textA <- renderText({
    if (nrow(dfB()) > 0) {
      paste("Team A receives", sum(dfB()$value))
    } else {
      "Input players for Team B"
    }
    
  })
  
  output$textB <- renderText({
    if (nrow(dfA()) > 0) {
      paste("Team B receives", sum(dfA()$value))
    } else {
      "Input players for Team A"
    }
    
  })
  
  
  #output$textB <- renderText({
  #  paste("Team B receives", sum(dfA()$value))
  #})
  
  
  df <- reactive({
    x$value <- round(10500 * exp(x$dynoECR * input$slider1))
    
    x <-
      x[(x$mergename %in% c(input$sideA)) |
          (x$mergename %in% c(input$sideB)), ]
  })
  
  output$results <- renderDT({
    datatable(df())
  })
  
})
