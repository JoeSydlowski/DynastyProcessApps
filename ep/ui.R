library(shiny)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)

df2019 <- read.csv("data2019cleaned.csv")


shinyUI(fluidPage(
  selectInput("selectTeam",
              "Select Team:",
              choices = c("All", as.character(sort(unique(df2019$posteam)))),
              selected = "CHI"),
  selectInput("selectPos",
              "Select Position:",
              choices = c("All", "QB", "RB", "WR", "TE"),
              selected = "All"),
  radioButtons("weeklyRadio",
               "Weekly or Cumulative?",
               choices = c("Weekly","Cumulative"),
               selected = "Cumulative"),
  conditionalPanel(condition = "input.weeklyRadio == 'Weekly'",
                   selectizeInput("selectWeeks",
                                  "Select Weeks:",
                                  choices = sort(unique(df2019$week)),
                                  selected = max(df2019$week),
                                  multiple = TRUE)),
  # selectizeInput("selectPlayers",
  #                "Select Players:",
  #                choices = c("All"),
  #                selected = "All",
  #                multiple = TRUE),
  DTOutput("teamTable")
))