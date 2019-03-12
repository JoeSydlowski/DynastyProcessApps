library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

namelist <- c("Andrew Luck")


shinyServer(function(input, output, session) {
  
  df <- reactive({
    
    dates <- tail(unique(x$date), input$numWeeks)
    
    x[(x$pos == input$posFilter) & (x$date %in% dates),]
  })
  
  observeEvent(input$posFilter,{
    #currentA <- input$sideA
    updateSelectizeInput(session, 'playerList',
                         choices = df()$name
                         #selected = c(currentA)
    )
  })
  
  output$distPlot <- renderPlot({
    
    dates <- tail(unique(x$date), input$numWeeks)

    sizes <- 4:(length(dates)+3)
    
    ggplot(df(), aes(dynpECR, rdpECR, group=name)) + 
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
      geom_text(data = df()[df()$name %in% input$playerList & df()$date == tail(dates, 1),],
                aes(dynpECR, rdpECR, label=name), nudge_x = -1, nudge_y = 1) +
      #geom_point_interactive(aes(tooltip = name)) +
      #coord_fixed(ratio = 1) +
      coord_equal() +
      theme_light()
  })
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 135, "px; top:", top_px + 90, "px; padding: 0;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })
  
  output$hover_info2 <- renderUI({
    hover <- input$plot_click
    point <- nearPoints(df(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 135, "px; top:", top_px + 90, "px; padding: 0;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })
  
})
