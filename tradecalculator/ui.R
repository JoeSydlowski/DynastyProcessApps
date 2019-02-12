library(shiny)
library(curl)
library(shinythemes)
library(DT)



x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15:18,56,57)
x <- x[cols]

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  column(10, offset = 4, titlePanel("DynastyProcess.com Trade Calculator")),
  hr(),
  sliderInput("slider1", "Depth Weight", min = -0.03, 
              max = -.01, value = -.024, step = 0.002),
  hr(),
  fluidRow(column(6,
          selectizeInput("sideA",
                 "Choose Assets from Team A:",
                 choices = x["mergename"],
                 multiple = TRUE)),
           column(6,
           selectizeInput("sideB",
                  "Choose Assets from Team B:",
                  choices = x["mergename"],
                  multiple = TRUE))),
  hr(),
  DTOutput("results")
  
           
  
))
