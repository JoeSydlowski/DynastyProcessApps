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

dates <- tail(unique(x$date),3)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  withTags(
    nav(class="navbar navbar-default navbar-static-top", role="navigation",
        div(class="container-fluid",
            div(class="navbar-header",
                span(class="navbar-brand",
                     a(href="https://dynastyprocess.com",strong("DynastyProcess.com"))
                )
            ),
            ul(class="nav navbar-nav",
               li(a(href="http://apps.dynastyprocess.com/database",strong("Database"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/calculator",strong("Calculator"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/rookie-adp",strong("Rookie ADP"))
               ),
               li(class="dropdown",
                  a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                  ul(class="dropdown-menu",
                     li(a(href="http://apps.dynastyprocess.com/ecr",strong("Arbitrage"))),
                     li(class="active",a(href="#",strong("ECR Explorer"))),
                     li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                  )
               )
            )
        )
    )
  ),
  titlePanel("DynastyProcess.com ECR Explorer"),
  shinyjs::useShinyjs(),
  id = "options",
  hr(),
  fluidRow(column(3,
                  radioGroupButtons("posFilter", "Choose a Position:",
                                    choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
                                    selected = "QB",
                                    status = "primary",
                                    justified = TRUE)),
           column(3,
                  sliderInput("playerRange",
                              "Select ECR Range:",
                              min = 1,
                              max = 64,
                              value = c(1,24)
                  )),
           column(3,
                  sliderTextInput("DateRange",
                                  "Select Date Range:",
                                  choices = unique(x$date),
                                  selected = unique(x$date)[c(length(unique(x$date)) - 5,length(unique(x$date)))]
                  )
           ),
           column(3,
                  selectizeInput("playerList",
                                 "Select Players",
                                 choices = c(x["name"]),
                                 selected = NULL,
                                 multiple = TRUE),
                  actionButton("clear1", "Reset To Defaults"))
  ),
  hr(),
  p("The ECR Explorer app is designed to compare trends in FantasyPros redraft and dynasty positional ranks. Players above the trendline are ranked higher in dynasty than in redraft, and below are ranked higher in redraft than dynasty. To zoom in on a specific area, click and drag to select the area and then double click to focus on that area."),
  hr(),
  #mainPanel(
  tabsetPanel(type = "tabs",
                tabPanel("Plot",
                         fluidRow(column(6,
                                          #offset=1,
                                         plotOutput("distPlot",
                                                    height = "600px",
                                                    # width = "100%",
                                                    dblclick = "dblclick",
                                                    brush = brushOpts(
                                                      id = "plot1_brush",
                                                      resetOnNew = TRUE),
                                                    
                                                    #width = "100%",
                                                    hover = hoverOpts(id= "plot_hover",
                                                                      delay = "100",
                                                                      delayType = "throttle"),
                                                    click = "plot_click")),
                                  column(6,#offset=1,
                                         div(DTOutput("printData")),class='font-size: small'),
                                  br(),
                                  downloadButton("downloadData1", "Download")
                                  ),
                         uiOutput("hover_info"),
                         uiOutput("hover_info2"))
                ,
                # tabPanel("Plot Data",
                #          br(),
                #          #downloadButton("downloadData1", "Download"),
                #          #br(),
                #          #br(),
                #          #fluidRow(
                #            column(9, DTOutput("printData")),
                #            column(2, downloadButton("downloadData1", "Download"),offset=1)
                #          #)
                # ),
                tabPanel("All Data",
                         br(),
                         #downloadButton("downloadData2", "Download"),
                         #br(),
                         #br(),
                         #fluidRow(
                           column(9, DTOutput("printData2")),
                           column(2, downloadButton("downloadData2", "Download"),offset=1)
                         #)
                )
    ))
  )
#)
#)
