library(shiny)
library(curl)
library(DT)
library(shinythemes)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))

shinyUI(fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("spacelab"),
  # h1(id = "big-heading",
  #    "DynastyProcess.com Database",
  #    align = "center"),
  # tags$style(HTML("#big-heading{color: white;
  #                 background-color:blue;
  #                 height: 100px;
  #                 font-size:60px;}")),
  
  
  column(10, offset = 4, titlePanel("DynastyProcess.com Database")),
  selectizeInput("select",
                 "Select columns:", 
                 choices = list(
                   bio = names(x)[12:18],
                   contract = names(x)[19:20],
                   fantasypros = names(x)[21:27],
                   draft = names(x)[28:31],
                   "season-stats-snaps" = names(x)[45:47],
                   "season-stats-passing" = names(x)[48:54],
                   "season-stats-rushing" = names(x)[55:60],
                   "season-stats-receiving" = names(x)[61:73],
                   "season-stats-firstdowns" = names(x)[74:76],
                   "season-stats-fantasy" = names(x)[77:78],
                   athletic = names(x)[32:44],
                   ID = names(x)[1:11]),
                 selected = c("mergename", "pos", "team", "age", "draft_year", "draft_round",
                              "offSnaps.", "offSnaps", "dynoECR", "dynpECR", "redpECR"),
                 multiple = TRUE,
                 width = "50%"),
  hr(),
  actionButton("clear1", "Reset All Filters"),
  downloadButton("downloadData", "Download"),
  hr(),
  DTOutput("results"),
  hr(),
  p(HTML(paste0("This app was created by ",
                a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
                " based on data from ",
                a(href = "https://dynastyprocess.com/downloads/database/", "DynastyProcess.com"),
                ". You can find the code at ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/arbitrage", "my github"),
                "."
  )))
))
