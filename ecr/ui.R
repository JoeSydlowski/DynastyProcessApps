library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fp_dynastyvsredraft.csv"),
              encoding = "unknown")

dates <- tail(unique(x$date),3)


shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("DynastyProcess.com ECR Explorer")),
  hr(),
  fluidRow(column(4,
                  radioButtons("posFilter", "Choose a Position:",
               choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
               selected = "QB")),
           column(4,
                  numericInput("numWeeks", "Choose how many weeks of data", value = 3,
                               min = 1, max = length(unique(x$date)), step = 1))),
  hr(),
  plotlyOutput("distPlot", width = "100%")
))
