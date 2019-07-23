library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(ggplot2)
library(rvest)
library(dplyr)

playerDB <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-players.csv"))
playerDB <- playerDB[c(1:6)]
pickDB <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-picks.csv"))

shinyServer(function(input, output, session) {
  
  combineddf <- reactive({
    names(pickDB)[1]<-"Name"
    x2020 <- dplyr::filter(pickDB, grepl('2020|2021', Name))
    x2019 <- dplyr::filter(pickDB, grepl('2019', Name))
    
    x2019$pickNum <- as.numeric(rownames(x2019)) %% as.numeric(input$leagueSize)
    x2019$pickNum[x2019$pickNum == 0] <- as.numeric(input$leagueSize)
    
    x2019$Name <- paste0("2019 Rookie Pick ",
                         ceiling(as.numeric(rownames(x2019))/as.numeric(input$leagueSize)),
                         ".",
                         sprintf("%02d", x2019$pickNum ))
    x2019$pickNum <- NULL
    pickDB <- rbind(x2019, x2020)
    #print(pickDB)
    
    pickDB$dynoECR <- ((1-input$slider2)*pickDB$min_dynoECR + input$slider2*pickDB$max_dynoECR)
    pickDB$dyno2QBECR <- ((1-input$slider2)*pickDB$min_dyno2QBECR + input$slider2*pickDB$max_dyno2QBECR)
    pickDB <- pickDB[c(1:4)]
    pickDB$team <- NA
    pickDB$age <- NA
    
    names(playerDB)[1]<-"Name"
    x <- rbind(playerDB, pickDB)
    
    x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400
    x
  })
  
  df <- reactive({
    x <- combineddf()
    
    if(input$numQB==TRUE) {
      x$dyno2QBECR <- NULL
      dftext <- "dynoECR"
    } else {
      x$dynoECR <- NULL
      dftext <- "dyno2QBECR"
    }
    
    
    x$value <- round(10500 * exp(x[,dftext]* input$slider1))
    x <- x[order(-x$value),]
    row.names(x) <- NULL
    
    if(input$calcType == "postdraft")
    { x2020 <- dplyr::filter(x, grepl('2020|2021', Name))
    x <- dplyr::filter(x, !grepl('2019|2020|2021', Name))
    
    x$pickNum <- as.numeric(rownames(x)) %% as.numeric(input$leagueSize)
    x$pickNum[x$pickNum == 0] <- as.numeric(input$leagueSize)
    
    x$Pick <- paste0( "Startup Pick ",
                      ceiling(as.numeric(rownames(x))/as.numeric(input$leagueSize)),
                      ".",
                      sprintf("%02d", x$pickNum ))
    x$Combined <- paste(x$Pick, x$Name, sep=" | ")
    x$Name <- x$Combined
    x$Combined <- x$Pick <- x$pickNum <- NULL
    #x2020$Combined <- x2020$Name
    x <- rbind(x, x2020)
    
    #x <- x[c(7,1,2,3,4,5,6)]
    }
    
    else if(input$calcType == "predraft")
    { x2020 <- dplyr::filter(x, grepl('2020|2021', Name))
    x <- dplyr::filter(x, !grepl('2020|2021', Name))
    
    x$pickNum <- as.numeric(rownames(x)) %% as.numeric(input$leagueSize)
    x$pickNum[x$pickNum == 0] <- as.numeric(input$leagueSize)
    
    x$Pick <- paste0( "Startup Pick ",
                      ceiling(as.numeric(rownames(x))/as.numeric(input$leagueSize)),
                      ".",
                      sprintf("%02d", x$pickNum ))
    x$Combined <- paste(x$Pick, x$Name, sep=" | ")
    x$Name <- x$Combined
    x$Combined <- x$Pick <- x$pickNum <- NULL
    #x2020$Combined <- x2020$Name
    x <- rbind(x, x2020)
    
    #x <- x[c(7,1,2,3,4,5,6)]
    }
    
    x <- x[order(-x$value),]
    x 
  })
  
  observeEvent({input$numQB
    input$calcType
    input$leagueSize
    input$slider1
    input$slider2},{
      
      # if(input$calcType != "normal")
      #   {choiceList = df()$Combined}
      # else {choiceList = df()$Name}
      
      
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
    #req(input$sideA)
    
    df()[(df()$Name %in% c(input$sideA)), ]
    
  })
  
  dfB <- reactive({
    #req(input$sideB)
    
    df()[(df()$Name %in% c(input$sideB)), ]
    
  })
  
  output$tableA <- renderTable({
    req(input$sideA)
    dftemp <- dfA()
    dftemp[,c(4,5)] <- lapply(dftemp[,c(4,5)], sprintf, fmt = "%4.1f")
    dftemp[,c(6)] <- lapply(dftemp[,c(6)], sprintf, fmt = "%4.0f")
    dftemp
  })
  
  output$tableB <- renderTable({
    req(input$sideB)
    dftemp <- dfB()
    dftemp[,c(4,5)] <- lapply(dftemp[,c(4,5)], sprintf, fmt = "%4.1f")
    dftemp[,c(6)] <- lapply(dftemp[,c(6)], sprintf, fmt = "%4.0f")
    dftemp
  })
  
  sumdfA <- reactive({
    sum(dfA()$value)
  })
  
  sumdfB <- reactive({
    sum(dfB()$value)
  })
  
  rawDiff <- reactive({
    if (sumdfA() > sumdfB())
    {sum(dfA()$value) - sum(dfB()$value)}
    else if (sum(dfA()$value) < sum(dfB()$value))
    {sum(dfB()$value) - sum(dfA()$value)}
    else
    {0}
  })
  
  percentDiff <- reactive({
    if (sumdfA() > sumdfB())
    {round(100*((sum(dfA()$value) - sum(dfB()$value))/sum(dfB()$value)))}
    else if (sum(dfA()$value) < sum(dfB()$value))
    {round(100*((sum(dfB()$value) - sum(dfA()$value))/sum(dfA()$value)))}
    else
    {0}
  })
  
  output$winner <- renderText({
    req(input$sideA, input$sideB)
    
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
    req(input$sideA, input$sideB)
    
    if (percentDiff()<5) {
      "This trade is approximately fair!"
    }
  })
  
  output$textA <- renderText({
    paste("Team A total", format(sum(dfA()$value), big.mark = ","))
  })
  
  output$textB <- renderText({
    paste("Team B total", format(sum(dfB()$value), big.mark = ","))
  })
  
  output$bar <- renderPlot({
    req(input$sideA, input$sideB)
    
    dfA_temp <- dfA()
    dfB_temp <- dfB()
    dfA_temp$Team <- "A"
    dfB_temp$Team <- "B"
    
    dfcomp <- rbind(dfA_temp,dfB_temp)
    
    
    ggplot(dfcomp, aes(x=Team, y=value, fill=Name)) + 
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Set1") +
      theme_light() +
      theme(text = element_text(size=20),
            legend.position="bottom")
    
  })
  
  closestObs <- reactive({
    
    which(abs(df()$value-rawDiff())==min(abs(df()$value-rawDiff())))
    
  })
  
  output$diffTable <- renderTable({
    req(input$sideA)
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
    #req(input$sideB)
    "Here are some options to even out the trade:"
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {"DynastyProcessCalculator.csv"},
    content = function(file) {write.csv(df(), file)}
  )
  
})
