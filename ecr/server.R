library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinyjs)
library(DT)
library(tidyr)
library(shinyWidgets)
library(grid)
library(shinylogs)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

x <- x[order(x$date, x$dynpECR),]

#Custom Table Container
createContainer <- function(dates){
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Player'),
        th(colspan = 2, dates[1], style="text-align:center"),
        th(colspan = 2, dates[2], style="text-align:center"),
        th(colspan = 2, dates[3], style="text-align:center")
        
      ),
      tr(
        lapply(rep(c('Dynasty', 'Redraft'), 3), th)
      )
    )
  ))
  return(sketch);
}

shinyServer(function(input, output, session) {
  
  track_usage(storage_mode = store_json(path = "logs/"))
  
  dateList <- reactive({
    allDates <- rev(unique(x$date))
    dates <- c(input$DateRange)
    minDate <<- which(allDates == dates[1])
    maxDate <<- which(allDates == dates[2])
    allDates[c(minDate,ceiling(mean(c(minDate,maxDate))),maxDate)]
  })
  
  observeEvent({input$posFilter
    input$DateRange},
    {
      currentA <- input$playerList
      dates <- dateList()
      updateSelectizeInput(session, 'playerList',
                           choices = unique(dfPos()["name"]),
                           selected = currentA
      )
    })
  
  observeEvent({input$posFilter
    input$DateRange},
    {
    names <- unique(dfPos()["name"])
      dates <- dateList()
      updateSliderInput(session, 'playerRange',
                           max = nrow(names))
    })
  
  dfPos <- reactive({
    dates <- dateList()
    x[(x$pos == input$posFilter) & (x$date %in% dates),]
    
  })
  
  dfNames <- reactive({
    dfOrder <- dfPos()[rev(order(as.Date(dfPos()$date), -dfPos()$dynpECR)),]
    dfOrder[input$playerRange[1]:input$playerRange[2],]
  })
  
  df <- reactive({
    dates <- dateList()
    if(is.null(input$playerList))
    {x[(x$name %in% dfNames()$name) & (x$date %in% dates),]}
    else
    {x[(x$name %in% input$playerList) & (x$date %in% dates),]}
    
  })
  
  observeEvent(input$clear1, {
    shinyjs::reset("options")
  })
  
  dfwide1 <- reactive({
    data <- df()
    colnames(data)[colnames(data)=="dynpECR"] <- "D"
    colnames(data)[colnames(data)=="rdpECR"] <- "R"
    
    wide <- data %>%
      select(name, date, D, R) %>%
      group_by(name) %>%
      gather(variable, value, D, R) %>%
      unite(temp, variable, date) %>%
      spread(temp, value)
    
    wide[,c(1,2,5,3,6,4,7)]
  })
  
  output$printData <- renderDT({ 
    DT::datatable(dfwide1() ,
                                options = list(
                                  pageLength = 25,
                                  order = list(5,'asc'),
                                  autoWidth = TRUE
                                  ),
    class = 'compact stripe',
    rownames= FALSE,
    container = createContainer( dateList()))
    })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {"DynastyProcessECR.csv"},
    content = function(file) {write.csv(dfwide1(), file)}
  )
  
  output$printData2 <- renderDT({ x },
                                filter='top',
                               options = list(pageLength = 25,
                                              order=list(list(3,'desc'),list(4,'asc')),
                                              autoWidth = TRUE
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
    if (is.infinite(max(ranges$xcoord,ranges$ycoord)))
    {rangeVec <- c(min(df()$dynpECR,df()$rdpECR),max(df()$dynpECR,df()$rdpECR))}
    else {rangeVec <- c(min(ranges$xcoord,ranges$ycoord),max(ranges$xcoord,ranges$ycoord))}
    
    while(rangeVec[2] - rangeVec[1] < 15)
    {if(rangeVec[1] <= 8)
        {rangeVec <- c(0,16)}
      else {
        rangeVec[1] <- rangeVec[1] - 1 
        rangeVec[2] <- rangeVec[2] + 1 }
    }

    dates <- dateList()
    
    sizes <- 4:(length(dates)+3)
    
    ggplot(df(), aes(dynpECR, rdpECR, group=name)) + 
      geom_point(aes(color=date, size=date)) +
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
            axis.title=element_text(size=16,face="bold"),
            legend.title = element_text(size=16),
            legend.text = element_text(size=14),
            legend.position="bottom") +
      xlab("Dynasty ECR") +
      ylab("Redraft ECR") +
      labs(color = "Dates", size = "Dates") +
      #expand_limits(x = c(0, max(16, ranges$xcoord)), y = c(0, max(16, ranges$ycoord))) +
      annotation_custom(textGrob("Win Now",x=0.95, y=0.1, hjust=1, vjust=0,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
      annotation_custom(textGrob("Dynasty Darlings",x=0.05, y=0.9, hjust=0, vjust=1,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
      #geom_point_interactive(aes(tooltip = name)) +
      coord_fixed(ratio = 1, xlim = rangeVec, ylim = rangeVec, expand = TRUE)
      #xlim(0, defaultSizelocal) +
      #ylim(0, defaultSizelocal) +
      #coord_cartesian(xlim = ranges$xcoord, ylim = ranges$ycoord, expand = TRUE)
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
    left_pct <- ((hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left))# - 0.3
    top_pct <- ((hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom))# + 0.28
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px +2, "px; top:", top_px +2, "px; padding: 0;")
    
    
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
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) #- 0.05
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) #+ 0.05
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; ; z-index:75; background-color: rgba(245, 245, 245, 0.70); ",
                    "left:", left_px +2, "px; top:", top_px +2 , "px; padding: 0;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> dynpECR: </b>", point$dynpECR, "<br/>",
                    "<b> rdpECR: </b>", point$rdpECR, "<br/>")))
    )
  })
  
})
