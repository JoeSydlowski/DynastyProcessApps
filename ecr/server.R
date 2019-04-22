library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(ggrepel)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

x <- x[order(x$name, x$date),]

namelist <- c("Andrew Luck")

shinyServer(function(input, output, session) {
  
  df <- reactive({
    
    dates <- tail(unique(x$date), input$numWeeks)
    
    x[(x$pos == input$posFilter) & (x$date %in% dates),]
  })
  
  ranges <- reactiveValues(xcoord = NULL, ycoord = NULL)
  xrange <- reactiveValues(x1 = 0, x2 = 220)
  yrange <- reactiveValues(y1 = 0, y2 = 220)
  
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
      #geom_line() +
      geom_path() +
      #geom_smooth(method='lm') +
      geom_abline() +
      scale_color_brewer(palette="Set1") +
      geom_text_repel(force = 5,
                      data = df()[df()$date == tail(dates, 1) & 
                                  xrange$x1 <= df()$dynpECR &
                                  df()$dynpECR  <= xrange$x2 &
                                  yrange$y1 <= df()$rdpECR &
                                  df()$rdpECR <= yrange$y2 ,],
                      aes(dynpECR, rdpECR, label=name)) +
      #geom_text(data = df()[df()$name %in% input$playerList & df()$date == tail(dates, 1),],
      #          aes(dynpECR, rdpECR, label=name), nudge_x = -1, nudge_y = 1) +
      #geom_point_interactive(aes(tooltip = name)) +
      #coord_fixed(ratio = 1) +
      #xlim(0, defaultSizelocal) +
      #ylim(0, defaultSizelocal) +
      coord_cartesian(xlim = ranges$xcoord, ylim = ranges$ycoord, expand = TRUE) +
      #coord_equal() +
      theme_light()
  })
  
  observeEvent(input$dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$xcoord <- c(brush$xmin, brush$xmax)
      ranges$ycoord <- c(brush$ymin, brush$ymax)
      xrange$x1 <- brush$xmin
      xrange$x2 <- brush$xmax
      yrange$y1 <- brush$ymin
      yrange$y2 <- brush$ymax
      
    } else {
      ranges$xcoord <- NULL
      ranges$ycoord <- NULL
      xrange$x1 <- 0
      xrange$x2 <- 220
      yrange$y1 <- 0
      yrange$y2 <- 220
    }
  })
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- ((hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)) - 0.3
    top_pct <- ((hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)) + 0.28
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    print(left_pct)
    print(left_px)
    print(top_pct)
    print(top_px)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; padding: 0;")
    
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
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) - 0.25
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) + 0.20
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; padding: 0;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })
  
})
