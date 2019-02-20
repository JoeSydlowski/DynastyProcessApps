library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18:27,30:78)
y <- x[cols]

y$draft_round[is.na(y$draft_round)] <- 8

shinyUI(fluidPage(
  theme = shinytheme("readable"),
  column(10, offset = 4, titlePanel("DynastyProcess.com Arbitrage App")),
  hr(),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["mergename"],
                 multiple = FALSE,
                 selected = "Tyler Boyd"),
  hr(),
  fluidRow(
    column(3,
           selectizeInput("selectcol",
                          "Select Comparison Variables:",
                          choices = list(
                            age = names(x)[18],
                            contract = names(x)[19:20],
                            fantasypros = names(x)[21:27],
                            draft = names(x)[30:31],
                            "season-stats-snaps" = names(x)[45:47],
                            "season-stats-passing" = names(x)[48:54],
                            "season-stats-rushing" = names(x)[55:60],
                            "season-stats-receiving" = names(x)[61:73],
                            "season-stats-firstdowns" = names(x)[74:76],
                            "season-stats-fantasy" = names(x)[77:78],
                            athletic = names(x)[32:44]),
                          multiple = TRUE,
                          selected = c("age", "draft_round", "tgts"))),
    column(3,
           radioButtons("numcomps", "Number of Comps",
                        choices = list("5" = 6, "10" = 11), 
                        selected = 6)),
    column(3,
           selectInput("presets", "Select a Recommended Set of Variables", 
                       choices = list("Default" = 0, "QB" = 1, "RB Opportunity" = 2,
                                      "RB Efficiency" = 3, "RB Athletic" = 4,
                                      "WR Opportunity" = 5, "WR Efficiency" = 6,
                                      "WR Athletic" = 7, "TE Opportunity" = 8,
                                      "TE Efficiency" = 9, "TE Athletic" = 10))),
    column(3,
           checkboxGroupInput("posFilter", "Positional Comps", 
                              choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"),
                              selected = c("QB","RB","WR","TE")))),
  hr(),
  uiOutput("intro"),
  hr(),
  textOutput("sampleSize"),
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
