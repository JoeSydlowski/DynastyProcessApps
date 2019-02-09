library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)


#setwd("~/Fantasy Football/Excel Sheets/DynastyProcess/ArbitrageApp")
options(warn=-1)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18,21:54,56:62)
y <- x[cols]

y$draft_round[is.na(y$draft_round)] <- 8

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("Arbitrage App")),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["mergename"],
                 multiple = FALSE,
                 selected = "Tyler Boyd"),
  hr(),
  selectizeInput("selectcol",
                 "Select Comparison Variables:",
                 choices = names(y)[2:ncol(y)],
                 multiple = TRUE,
                 selected = c("age", "draft_round", "tgts")),
  hr(),
  textOutput("sampleSize"),
  hr(),
  DTOutput("results")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

