library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinyjs)
library(DT)
library(tidyr)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

x <- x[order(x$date, x$dynpECR),]

#x <- x[order(x$name, x$date),]

shinyServer(function(input, output, session) {
  
  dateList <- reactive({
    allDates <- rev(unique(x$date))
    dates <- c(input$DateRange)
    minDate <<- which(allDates == dates[1])
    maxDate <<- which(allDates == dates[2])
    allDates[c(minDate,ceiling(mean(c(minDate,maxDate))),maxDate)]
  })
  
  dfNames <- reactive({
    dates <- dateList()
    x[(x$pos == input$posFilter) & (x$date %in% dates),]
  })
  
  observeEvent({input$posFilter
                input$DateRange},
    {
    currentA <- input$playerList
    dates <- dateList()
    updateSelectizeInput(session, 'playerList',
                         choices = unique(dfNames()["name"]),
                         selected = currentA)
  })
  
  observeEvent({input$posFilter
    input$DateRange},
    {
    names <- unique(dfNames()["name"])
      dates <- dateList()
      updateSliderInput(session, 'playerRange',
                           max = nrow(names))
    })
  
  df <- reactive({
    playerNames <- dfNames()[input$playerRange[1]:input$playerRange[2],]
    
    if(is.null(input$playerList))
    {dfNames()[dfNames()$name %in% playerNames$name,]}
    else
    {dfNames()[dfNames()$name %in% input$playerList,]}
  })
  
  observeEvent(input$clear1, {
    shinyjs::reset("options")
  })
  
  dfwide1 <- reactive({
    df() %>%
      select(name, date, dynpECR, rdpECR) %>%
      group_by(name) %>%
      gather(variable, value, dynpECR, rdpECR) %>%
      unite(temp, variable, date) %>%
      spread(temp, value)
  })
  
  output$printData <- renderDT({ dfwide1() },
    options = list(pageLength = 25,
                   autoWidth = TRUE
                   #columnDefs = list(list(width = '200px', targets = "_all"))
    ),
    class = 'compact stripe',
    rownames= FALSE
    )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {"DynastyProcessECR.csv"},
    content = function(file) {write.csv(dfwide1(), file)}
  )
  
  output$printData2 <- renderDT({ x },
                               options = list(pageLength = 25,
                                              autoWidth = TRUE,
                                              scrollX = TRUE
                                              #columnDefs = list(list(width = '200px', targets = "_all"))
                               ),
                               class = 'compact stripe',
                               rownames= FALSE
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {"DynastyProcessAllECR.csv"},
    content = function(file) {write.csv(x, file)}
  )
  
  ranges <- reactiveValues(xcoord = NULL, ycoord = NULL)
  xrange <- reactiveValues(x1 = 0, x2 = 220)
  yrange <- reactiveValues(y1 = 0, y2 = 220)
  
  output$distPlot <- renderPlot({
    #req(input$playerList)
    
    #dates <- tail(unique(x$date), 3) #input$numWeeks)

    dates <- dateList()
    
    sizes <- 4:(length(dates)+3)
    
    ggplot(df(), aes(dynpECR, rdpECR, group=name)) + 
      geom_point(aes(color=date, size=date)) +
      #geom_point(aes(color=date, size=date), shape = 1, color = "black") +
      scale_size_manual( values = sizes) +
      geom_path() +
      geom_abline() +
      scale_color_brewer(palette="Set1") +
      geom_text_repel(force = 10,
                      data = . %>% 
                        mutate(label = ifelse(df()$date == tail(dates, 1) & 
                                  xrange$x1 <= df()$dynpECR &
                                  df()$dynpECR  <= xrange$x2 &
                                  yrange$y1 <= df()$rdpECR &
                                  df()$rdpECR <= yrange$y2,
                                  as.character(df()$name), "")),
                      aes(label = label), 
                      box.padding = 0.5,
                      segment.color = "grey50") +
      theme_light() + 
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold")) +
      xlab("Dynasty ECR") +
      ylab("Redraft ECR")+
      expand_limits(x = c(0, max(16, ranges$xcoord)), y = c(0, max(16, ranges$ycoord))) +

      #geom_point_interactive(aes(tooltip = name)) +
      #coord_fixed(ratio = 1) +
      #xlim(0, defaultSizelocal) +
      #ylim(0, defaultSizelocal) +
      coord_cartesian(xlim = ranges$xcoord, ylim = ranges$ycoord, expand = TRUE)
      #coord_equal() +
      
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
