library(curl)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(shinythemes)

#x <- read.csv("C:/Users/syd23/Documents/Fantasy Football/Excel Sheets/FFStatistics/qbdata.csv")
x <- read.csv("qbdata.csv")

cols <- c("Player", "Season", "Age", "Overall", "PosRank.4pt.TD.")
y <- x[cols]


shinyUI(fluidPage(
  theme = shinytheme("readable"),
  column(10, offset = 4, titlePanel("DynastyProcess.com Cohort App")),
  hr(),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["Player"],
                 multiple = FALSE,
                 selected = "Patrick Mahomes"),
  hr(),
  DTOutput("results")
  
  
))
