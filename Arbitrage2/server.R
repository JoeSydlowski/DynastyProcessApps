#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18,21:54,56:62)
y <- x[cols]

y$draft_round[is.na(y$draft_round)] <- 8


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  df1 <- reactive({
    
    z <- y[,input$selectcol, drop = FALSE]
    
    threshold <- ifelse(ncol(z) <= 4, 0.1, 0.2)
    
    z <- cbind(pos = x$pos, z)
    z <- cbind(mergename = x$mergename, z)
    
    z <- z[!rowSums(is.na(z)) > ncol(z)*threshold,]
  })
  
  zsize <- reactive({
    
    nrow(df1())
    
  })
  
  df <- reactive({
    z_norm <- df1() %>% mutate_at(funs(scale(.) %>% as.vector), .vars=vars(3:ncol(df1())))
    
    z_norm$mergename <- as.character(z_norm$mergename)
    z_norm$mergename[(z_norm$mergename == "Ryan Griffin" & z_norm$pos == "QB")] <- "Ryan Griffin QB"
    z_norm$mergename[(z_norm$mergename == "Ryan Griffin" & z_norm$pos == "TE")] <- "Ryan Griffin TE"
    
    rownames(z_norm) <- z_norm$mergename
    z_norm <- z_norm[,3:ncol(z_norm)]
    
    playername <- input$selected
    
    df <- as.data.frame(as.matrix(dist(z_norm[])))
    
    newdata <- df[order(df[[playername]]),]
    
    newdata <- rownames_to_column(newdata, "Name")[1:6,]
    
    newdata <- newdata[c("Name")]
    
    merge <- left_join(newdata, x, by = c("Name"="mergename"))
    
    merge <- merge[c("Name", "pos", "team", "dynpECR", "dynoECR", input$selectcol)]
    
    data.frame(merge)
    
  })
  
  output$results <- renderDT({
    datatable( df())
  })
  
  output$sampleSize <- renderText({ 
    paste("Your sample size is", zsize())
  })  
  
})
