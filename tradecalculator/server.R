library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(ggplot2)

#x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values.csv"))

#cols <- c(15:18, 56, 57)
cols <- c(1:4,6,7)

x <- x[cols]
names(x)[1]<-"Name"
x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400

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
                         choices = df()$Name,
                         selected = c(currentA)
    )
    currentB <- input$sideB
    updateSelectizeInput(session, 'sideB',
                         choices = df()$Name,
                         selected = c(currentB)
    )
  })
  
  dfA <- reactive({
    req(input$sideA)
    
    df()[(df()$Name %in% c(input$sideA)), ]
    
  })
  
  dfB <- reactive({
    req(input$sideB)
    
    df()[(df()$Name %in% c(input$sideB)), ]
    
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
      paste0("Side A is winning the trade by ",
            format(sum(dfA()$value) - sum(dfB()$value), big.mark = ","),
            " or ", percentDiff(), "%")
    } else if (sum(dfA()$value) < sum(dfB()$value)) {
      paste0("Side B is winning the trade by ",
            format(sum(dfB()$value) - sum(dfA()$value), big.mark = ","),
            " or ", percentDiff(), "%")
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
    paste("Team A total", format(sum(dfB()$value), big.mark = ","))
  })
  
  output$textB <- renderText({
    paste("Team B total", format(sum(dfA()$value), big.mark = ","))
  })
  
  output$bar <- renderPlot({
    dfA_temp <- dfA()
    dfB_temp <- dfB()
    dfA_temp$Team <- "A"
    dfB_temp$Team <- "B"
    
    dfcomp <- rbind(dfA_temp,dfB_temp)
    

    ggplot(dfcomp, aes(x=Team, y=value, fill=Name)) + 
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Set1") +
      theme(text = element_text(size=20))

  })
  
  closestObs <- reactive({

    which(abs(df()$value-rawDiff())==min(abs(df()$value-rawDiff())))
    
  })
  
  output$diffTable <- renderTable({
    rowObs <- closestObs()

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
