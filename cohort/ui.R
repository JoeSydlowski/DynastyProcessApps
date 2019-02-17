library(curl)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(shinythemes)


x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess-private/master/datasets/ffstats-feb2019/QBdata.csv?token=AtNYwWF-d4Kr_8OGaqPbzrXLhVrwPa_yks5caP09wA%3D%3D"))

cols <- c("Player", "Season", "Age", "Overall", "Positional.4")
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
