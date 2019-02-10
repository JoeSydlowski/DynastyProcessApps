library(shiny)
library(curl)
library(DT)
library(shinythemes)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols = c(24:32,46:51)

# Define UI for application that draws a histogram
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
                   PlayerVariables = names(x)[12:22],
                   Fantasypros = names(x)[56:62],
                   Contract = names(x)[54:55],
                   Snaps = names(x)[52:53],
                   Stats2018 = names(x)[cols],
                   Vitals = names(x)[33:45],
                   PlayerIDs = names(x)[1:11]),
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
                " and you can find the code at ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/arbitrage", "my github."),
                " The data for this page is provided by ",
                a(href = "https://dynastyprocess.com/downloads/database/", "https://dynastyprocess.com/downloads/database/"),
                " created by ",
                a(href = "https://twitter.com/_TanHo", "Tan Ho."),
                " Head there to see the full documentation for the datbase.")))
))
