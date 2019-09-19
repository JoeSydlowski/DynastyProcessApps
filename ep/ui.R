library(shiny)
library(dplyr)
#library(nflscrapR)
library(DT)
library(tidyr)
df2019 <- read.csv("data2019.csv")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  selectInput("selectTeam",
              "Select Team:",
              choices = c("All", as.character(sort(unique(df2019$posteam)))),
              selected = "CHI"),
  radioButtons("weeklyRadio",
               "Weekly or Cumulative?",
               choices = c("Weekly","Cumulative")),
  DTOutput("teamTable")
))
