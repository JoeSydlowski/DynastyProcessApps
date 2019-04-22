library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(rvest)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-players.csv"))

webpage <- read_html('https://github.com/tanho63/dynastyprocess/blob/master/files/values.csv')
lastupdate <- webpage %>%
  html_nodes("relative-time") %>%
  html_text()

cols <- c(1:6)

x <- x[cols]
names(x)[1]<-"Name"
x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400

shinyUI(fluidPage(
  theme = shinytheme("readable"),
  #shinythemes::themeSelector(),
  column(10, offset = 4, titlePanel("DynastyProcess.com Trade Calculator")),
  hr(),
  fluidRow(column(4,
                  radioButtons("numQB", "Choose League Type",
                               choices = list("1QB" = "dynoECR", "2QB / Superflex" = "dyno2QBECR"), 
                               selected = "dynoECR")),
           column(4,
                  radioButtons("calcType", "Startup Mode",
                               choices = list("Normal" = "normal",
                                              "Startup (Players and 2019 Picks)" = "predraft",
                                              "Startup (Players Only)" = "postdraft"),
                               selected= "predraft")),
           column(4,
                  selectInput("leagueSize", "Number of Teams (Startup Mode only)",
                              choices = list("8" = 8,
                                             "10" = 10,
                                             "12" = 12,
                                             "14" = 14,
                                             "16" = 16),
                              selected = 12))),
  fluidRow(         
           column(4, 
                  sliderInput("slider1", "Depth Weight", min = -0.03,
                              max = -.02, value = -.024, step = 0.001,
                              label = div(style='width: 500px ; max-width: 100% ;' , 
                                          div(style='float:left;', 'Studs-Heavy'), 
                                          div(style='float:right;', 'Depth-Heavy')),
                              width= '500px'
                  )),
           column(4,
                  sliderInput("slider2", "Pick Value", min = 0,
                              max = 1, value = 0.5, step = 0.1,
                              label = div(style='width: 500px ; max-width: 100% ;' , 
                                          div(style='float:left;', 'Pick Skeptic'), 
                                          div(style='float:right;', 'Pick Optimist')),
                              width= '500px'
                  )),
           column(4,
                  downloadButton("downloadData", "Download Values"))),
  hr(),
  fluidRow(column(6,
                  selectizeInput("sideA",
                                 "Team A:",
                                 choices = x["Name"],
                                 multiple = TRUE)),
           column(6,
                  selectizeInput("sideB",
                                 "Team B:",
                                 choices = x["Name"],
                                 multiple = TRUE))),
  hr(),
  column(10, offset = 4, h3(textOutput("winner"))),
  column(10, offset = 4, h3(textOutput("winRange"))),
  fluidRow(column(6,
                  h4(textOutput("textA")),
                  tableOutput("tableA")),
           column(6,
                  h4(textOutput("textB")),
                  tableOutput("tableB"))),
  hr(),
  fluidRow(column(6,
                  plotOutput("bar")),
           column(6,
                  h4(textOutput("tableText")),
                  tableOutput("diffTable"))),
  hr(),
  p(paste0("Player values last updated on ", lastupdate, ".")),
  p(HTML(paste0("This app was created by ",
                a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
                " based on data from ",
                a(href = "https://dynastyprocess.com/downloads/values/", "DynastyProcess.com"),
                ". You can find the code at ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/arbitrage", "my github"),
                ".")))
))
