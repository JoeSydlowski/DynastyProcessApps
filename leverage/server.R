library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

#x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
#cols <- c(15:18,56:62)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"))

dates <- tail(unique(x$date),3)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    ggplotly(ggplot(x[(x$pos == input$posFilter) &
               (x$date %in% dates),], aes(dynpECR, rdpECR, group=X.U.FEFF.name)) + 
      geom_point(aes(color=date), size = 2) +
      geom_point(shape = 1, color = "black", size = 2) +
      geom_line() +
      #geom_smooth(method='lm')
      geom_abline() +
      scale_color_manual(values=c("#FA8072", "#FDFD71", "#32CD32")) +
      theme_light())  %>% layout(height = 750)
  }) 
  
})
