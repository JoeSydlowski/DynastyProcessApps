library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18:27,30:78)
y <- x[cols]

cols2 <- c(27:48, 51:60)
y$draft_round[is.na(y$draft_round)] <- 8
y[cols2][is.na(y[cols2])] <- 0

shinyServer(function(input, output, session) {
  
  outVar <- reactive({
    if (input$presets == 0) {c("age", "draft_round", "wopr", "ppr.g", "adot")}
    else if (input$presets == 1) {c("age", "draft_round", "paATTs", "pa_aya", "ppr.g")}
    else if (input$presets == 2) {c("draft_round", "ms_tgts", "ruATTs", "offSnapsPct")}
    else if (input$presets == 3) {c("ruTDs", "reTDs", "ppr.g", "adot")}
    else if (input$presets == 4) {c("age", "forty", "cone", "height")}
    else if (input$presets == 5) {c("draft_round", "wopr", "adot", "offSnapsPct")}
    else if (input$presets == 6) {c("racr", "reTDs", "ppr.g", "adot")}
    else if (input$presets == 7) {c("age", "forty", "cone", "height", "shuttle")}
    else if (input$presets == 8) {c("draft_round", "wopr", "adot", "offSnapsPct")}
    else if (input$presets == 9) {c("racr", "reTDs", "ppr.g", "adot")}
    else if (input$presets == 10) {c("age", "forty", "cone", "height", "broad")}
  })
  
  observeEvent(input$presets, {
    updateSelectizeInput(session, "selectcol", selected = outVar())
    })

  df1 <- reactive({
    req(input$selected)
    req(input$selectcol)
    
    z <- y[,input$selectcol, drop = FALSE]
    
    threshold <- ifelse(ncol(z) < 4, 0.1, 0.25)
    
    z <- cbind(pos = x$pos, z)
    z <- cbind(mergename = x$mergename, z)
    
    z <- z[!rowSums(is.na(z)) > ncol(z)*threshold,]

    z_norm <- z %>% mutate_at(funs(scale(.) %>% as.vector), .vars=vars(3:ncol(z)))
    
    z_norm$mergename <- as.character(z_norm$mergename)
    z_norm$mergename[(z_norm$mergename == "Ryan Griffin" & z_norm$pos == "QB")] <- "Ryan Griffin QB"
    z_norm$mergename[(z_norm$mergename == "Ryan Griffin" & z_norm$pos == "TE")] <- "Ryan Griffin TE"
    
    rownames(z_norm) <- z_norm$mergename
    z_norm <- z_norm[,3:ncol(z_norm)]
    
    playername <- input$selected
    
    df <- as.data.frame(as.matrix(dist(z_norm[])))
    
    newdata <- df[order(df[[playername]]),]
    
    newdata <- rownames_to_column(newdata, "Name")
    
    cols <- c("Name", playername)
    
    newdata <- newdata[cols]
    
    names(newdata)[2]<-"Match"
    
    newdata$MatchRating <- round(100-(100/(max(newdata$Match, na.rm=TRUE))*newdata$Match))
    
    merge <- left_join(newdata, x, by = c("Name"="mergename"))
    
    merge <- merge[unique(c("Name", "MatchRating", "pos", "team", "dynpECR", "dynoECR", input$selectcol))]
    
    merge <- merge[(merge$pos %in% c(input$posFilter)) | (merge$Name == playername),]
  })
  
  zsize <- reactive({
    
    nrow(df1())
    
  })
  
  df <- reactive({
    
    merge <- df1()[1:input$numcomps,]
    
    data.frame(merge)
    
  })
  
  output$results <- renderDT({
    brks <- c("69","79","84","89","94")
    clrs <- c("rgba(255, 255, 255, .4)",
              "rgb(255,82,51)",
              "rgb(255,139,51)",
              "rgb(254,255,51)",
              "rgb(50,205,50)",
              "rgb(34,139,34)")
    
    datatable( df(),
               options = list(pageLength = 11,
                              scrollX =TRUE, 
                              dom = 't')) %>% formatStyle(
                                'MatchRating',
                                backgroundColor = styleInterval(brks, clrs))
  })
    
  output$intro <- renderUI({ 
    HTML(paste("The Arbitrage App helps you find comparable players based on any criteria available in the ",
                a(href = "https://dynastyprocess.com/database", "DynastyProcess.com Database."),
                "These comparisons might provide possible pivots on a player you are trying to buy or sell.
                 For more information, check out the readme on ",
                a(href = "https://dynastyprocess.com/arbitrage", "DynastyProcess.com.")
               ))
  })  
  
  output$sampleSize <- renderText({ 
    paste("Your current sample size is", zsize(),".")
  })  
  
})
