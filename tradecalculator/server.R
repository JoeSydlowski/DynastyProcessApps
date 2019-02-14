library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(ggplot2)

options(shiny.reactlog=TRUE) 

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18, 56, 57)
x <- x[cols]
x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400


#ggplot(x, aes(x=team, y=dynoECR, fill=mergename)) + 
#  geom_bar(stat="identity")


shinyServer(function(input, output, session) {
  
  df <- reactive({
    if(input$numQB=="dynoECR") {
      x$dyno2QBECR <- NULL
    } else {
      x$dynoECR <- NULL
    }
    
    x$value <- round(10500 * exp(x[,input$numQB]* input$slider1))
    
    x <- x[order(-x$value),] 
  })
  
  observeEvent(input$numQB,{
    currentA <- input$sideA
    updateSelectizeInput(session, 'sideA',
                         choices = df()$mergename,
                         selected = c(currentA)
    )
    currentB <- input$sideB
    updateSelectizeInput(session, 'sideB',
                         choices = df()$mergename,
                         selected = c(currentB)
    )
  })
  
  dfA <- reactive({
    req(input$sideA)
    
    df()[(df()$mergename %in% c(input$sideA)), ]
    
  })
  
  dfB <- reactive({
    req(input$sideB)
    
    df()[(df()$mergename %in% c(input$sideB)), ]
    
  })
  
  output$tableA <- renderTable({
    dfA()},
    digits = 0
  )
  
  output$tableB <- renderTable({
    dfB()},
    digits = 0
  )
  
  sumdfA <- reactive({
    sum(dfA()$value)
  })
  
  sumdfB <- reactive({
    sum(dfB()$value)
  })
  
  rawDiff <- reactive({
    if (sumdfA() > sumdfB()) {
      sum(dfA()$value) - sum(dfB()$value)
    } else if (sum(dfA()$value) < sum(dfB()$value)) {
      sum(dfB()$value) - sum(dfA()$value)
    }
  })
  
  percentDiff <- reactive({
    if (sumdfA() > sumdfB()) {
      round(100*((sum(dfA()$value) - sum(dfB()$value))/sum(dfB()$value)))
    } else if (sum(dfA()$value) < sum(dfB()$value)) {
      round(100*((sum(dfB()$value) - sum(dfA()$value))/sum(dfA()$value)))
    }
  })
  
  output$winner <- renderText({
    if (sumdfA() > sumdfB()) {
      paste("Side A is winning the trade by",
            format(sum(dfA()$value) - sum(dfB()$value), big.mark = ","),
            "or", percentDiff(), "%")
    } else if (sum(dfA()$value) < sum(dfB()$value)) {
      paste("Side B is winning the trade by",
            format(sum(dfB()$value) - sum(dfA()$value), big.mark = ","),
            "or", percentDiff(), "%")
    } else {
      "This trade is exactly even!"
    }
  })
  
  output$winRange <- renderText({
    if (percentDiff()<5) {
      "This trade is approximately fair!"
    }
  })
  
  output$textA <- renderText({
    paste("Team A receives", format(sum(dfB()$value), big.mark = ","))
  })
  
  output$textB <- renderText({
    paste("Team B receives", format(sum(dfA()$value), big.mark = ","))
  })
  
  output$bar <- renderPlot({
    dfA_temp <- dfA()
    dfB_temp <- dfB()
    dfA_temp$Team <- "A"
    dfB_temp$Team <- "B"
    
    dfcomp <- rbind(dfA_temp,dfB_temp)
    

    ggplot(dfcomp, aes(x=Team, y=value, fill=mergename)) + 
      geom_bar(stat="identity")
    
  })
  
  closestObs <- reactive({
    print(rawDiff())
    print(df()$value)
    
    which(abs(df()$value-rawDiff())==min(abs(df()$value-rawDiff())))
    
  })
  
  output$diffTable <- renderTable({
    rowObs <- closestObs()
    print(rowObs)
    
    if (rowObs <=5) {
      rowRange <- c(1:10)
    } else {
      upper <- rowObs-4
      lower <- rowObs+5
      rowRange <- c(upper:lower)
    }
    
    df()[rowRange,]},
    digits = 0
    )
  
  output$tableText <- renderText({
    req(input$sideA)
    req(input$sideB)
    "Here are some options to even out the trade."
  })
  
})
