library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18,21:54,56:62)
y <- x[cols]

y$draft_round[is.na(y$draft_round)] <- 8

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("DynastyProcess.com Arbitrage App")),
  hr(),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["mergename"],
                 multiple = FALSE,
                 selected = "Tyler Boyd"),
  hr(),
  fluidRow(
    column(4,
           selectizeInput("selectcol",
                          "Select Comparison Variables:",
                          choices = names(y)[2:ncol(y)],
                          multiple = TRUE,
                          selected = c("age", "draft_round", "tgts"))),
    column(4,
    radioButtons("numcomps", "Number of Comps",
               choices = list("5" = 6, "10" = 11), 
               selected = 6)),
    column(4,
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
