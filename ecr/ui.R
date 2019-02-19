library(shiny)
library(curl)
library(shinythemes)
library(ggplot2)
library(plotly)

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("DynastyProcess.com ECR Explorer")),
  hr(),
  radioButtons("posFilter", "Choose a Position:",
               choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"), 
               selected = "QB"),
  hr(),
  plotlyOutput("distPlot")
))
