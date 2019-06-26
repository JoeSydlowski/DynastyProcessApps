library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(dplyr)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

x <- x[order(x$date, x$dynpECR),]

#x <- x[order(x$name, x$date),]

namelist <- c("Andrew Luck")

shinyServer(function(input, output, session) {
  
  #Update Player list based on position filter
  observeEvent(input$posFilter,{
    currentA <- input$playerList
    updateSelectizeInput(session, 'playerList',
                         choices = list("Presets" = c("All", "1-24", "25-48", "49+"),
                                        "Players" = df2()["name"]),
                         selected = c(currentA)
    )
  })
  
  #Update Player list based on player ranges
  observeEvent(input$playerList,{
    currentA <- input$playerList
    
    if (currentA[1] == "All" & length(currentA) > 1)
    {currentA <- currentA[!currentA %in% "All"]}
    
    if("All" %in% currentA & currentA[1] != "All")
    {currentA <- c("All")}
    
    if ("1-24" %in% currentA & "25-48" %in% currentA & "49+" %in% currentA)
    {currentA <- c("All")}
    
    updateSelectizeInput(session, 'playerList',
                         choices = list("Presets" = c("All", "1-24", "25-48", "49+"),
                                        "Players" = df2()["name"]),
                         selected = c(currentA)
    )
  })
  
  dateList <- reactive({
    allDates <- rev(unique(x$date))
    dates <- c(input$DateRange)
    minDate <<- which(allDates == dates[1])
    maxDate <<- which(allDates == dates[2])
    allDates[c(minDate,ceiling(mean(c(minDate,maxDate))),maxDate)]
  })
  
  df <- reactive({
    #req(input$playerList)
    
    maxDate <- max(as.Date(dateList()))
    
    if (input$playerList[1] == "All")
    {x[(x$pos == input$posFilter) & (x$date %in% dateList()),]}
    
    else if ("1-24" %in% input$playerList & "25-48" %in% input$playerList)
    {df1 <- x[(x$pos == input$posFilter) & (x$date == as.character(maxDate)),]
    players <- df1 %>% slice(1:48)
    x[(x$pos == input$posFilter) & (x$date %in% dateList() & (x$name %in% players$name)),]
    }
    
    else if (input$playerList[1] == "1-24")
    {df1 <- x[(x$pos == input$posFilter) & (x$date == as.character(maxDate)),]
     players <- df1 %>% slice(1:24)
     x[(x$pos == input$posFilter) & (x$date %in% dateList() & (x$name %in% players$name)),]
    }
    
    else if (input$playerList[1] == "25-48")
    {df1 <- x[(x$pos == input$posFilter) & (x$date == as.character(maxDate)),]
    players <- df1 %>% slice(25:48)
    x[(x$pos == input$posFilter) & (x$date %in% dateList() & (x$name %in% players$name)),]
    }
    
    else if (input$playerList[1] == "49+")
    {df1 <- x[(x$pos == input$posFilter) & (x$date == as.character(maxDate)),]
    players <- df1 %>% slice(49:nrow(df1))
    x[(x$pos == input$posFilter) & (x$date %in% dateList() & (x$name %in% players$name)),]
    }
    
    else
    {x[(x$pos == input$posFilter) & (x$date %in% dateList() & (x$name %in% input$playerList)),]}
    
  })
  
  df2 <- reactive({
    
    #dates <- tail(unique(x$date), 3)#input$numWeeks)
    
    dates <- dateList()

    x[(x$pos == input$posFilter) & (x$date %in% dates) & !(x$name %in% df()$name),]
  })
  
  ranges <- reactiveValues(xcoord = NULL, ycoord = NULL)
  xrange <- reactiveValues(x1 = 0, x2 = 220)
  yrange <- reactiveValues(y1 = 0, y2 = 220)
  
  output$distPlot <- renderPlot({
    req(input$playerList)
    
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
