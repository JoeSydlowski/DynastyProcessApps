library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18,56:62)

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("DynastyProcess.com ECR App")),
  hr(),
  radioButtons("posFilter", "Choose a Position:",
               choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
               selected = "QB"),
  hr(),
  plotlyOutput("distPlot")
))
