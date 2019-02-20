library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")


shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    dates <- tail(unique(x$date), input$numWeeks)
    
    df <- x[(x$pos == input$posFilter) & (x$date %in% dates),]
    
    sizes <- 2:(length(dates)+1)
    
    p <- ggplot(df, aes(dynpECR, rdpECR, group=name)) + 
      geom_point(aes(color=date, size=date)) +
      #geom_point(aes(color=date, size=date), shape = 1, color = "black") +
      scale_size_manual( values = sizes) +
      #geom_text(aes(label = ifelse(date == tail(dates,1), as.character(name), ""))) +
      geom_line() +
      #geom_smooth(method='lm') +
      geom_abline() +
      scale_color_brewer(palette="Set1") +
      #scale_color_manual(values=c("#FA8072", "#FDFD71", "#32CD32")) +
      #scale_color_manual(values=c("grey","grey","blue")) +
      theme_light()
    
    ggplotly(p, tooltip = c("name", "rdpECR", "dynpECR"),
             height = 650, dynamicTicks = TRUE) %>%
      style(textposition = "bottom-right")
    
  })
  
  
  # output$distPlot2 <- renderPlotly({
  #   plot_ly(x[(x$pos == input$posFilter) &
  #               (x$date %in% dates),],
  #           x = ~dynpECR,
  #           y = ~rdpECR,
  #           type = "scatter",
  #           color = ~date,
  #           colors = c("red", "yellow", "green"),
  #           mode = 'markers',
  #           sizes = c(5,10,20),
  #           group = ~date
  #           #marker = list()
  #           #colors = c("#FA8072", "#FDFD71", "#32CD32")
  #   )
  #}) 
  
  
})
