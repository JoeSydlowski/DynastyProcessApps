library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  selectInput("selectTeam","Select Team:",
              choices = unique(df2019$posteam),
              selected = "CHI"),
  DTOutput("teamTable")
))
