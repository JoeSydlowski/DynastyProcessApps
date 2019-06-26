library(shiny)
library(curl)
library(DT)
library(dplyr)
library(anytime)


x <- read.csv(curl("https://raw.githubusercontent.com/JoeSydlowski/DynastyProcess/master/rookie-adp/adp-picks.csv"))
#x <- read.csv(file="adp-picks.csv")
x$pick_timestamp <- ifelse(x$MFL_Sleeper == "Sleeper", x$pick_timestamp/1000, x$pick_timestamp)
x$date <- anydate(x$pick_timestamp)
min(x$date)

shinyServer(function(input, output) {
  QBs <- reactive({
    if(substring(input$mode, 1, 1) == "1")
      {"1QB"}
    else if(substring(input$mode, 1, 1) == "2")
      {"2QB/SF"}
  })
  
  OffDef <- reactive({
    if(substring(input$mode, 5, 5) == "I")
      {"IDP"}
    else if (substring(input$mode, 5, 5) == "O")
      {"Offense-only"}
  })
  
  
  df <- reactive({
    df1 <- x %>%
      filter(QB_type == QBs() &
               IDP %in% OffDef() &
               date <= input$dateRange[2] &
               date >= input$dateRange[1] &
               name != "") %>%
      group_by(name, pos, tm) %>%
      summarise(ADP = round(mean(pick),1),
                Count = n(),
                High = min(pick),
                Low = max(pick),
                SD = round(sd(pick),1),
                CV = round(sd(pick)/mean(pick),1)) %>%
      arrange(ADP)
    
    print(sapply(df1$ADP, typeof))
    
    df1
  })  

  output$results <- renderDT({
    datatable( df(),
               filter = 'top',
               #rowname = FALSE,
               options = list(pageLength = 50,
                              scrollX = TRUE,
                              columnDefs = list(list(className = 'dt-head-left', targets = "_all"))),
               class = 'compact stripe')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {"DynastyProcessRookieADP.csv"},
    content = function(file) {write.csv(df(), file)}
  )
  
  proxy = dataTableProxy('results')
  
  observeEvent(input$clear1, {proxy %>% clearSearch()}
  )
  
})