library(shiny)
library(curl)
library(shinythemes)
library(DT)

#x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values.csv"))

#cols <- c(15:18, 56, 57)
cols <- c(1:4,6,7)

x <- x[cols]
names(x)[1]<-"Name"
x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400

shinyUI(fluidPage(
  theme = shinytheme("readable"),
  #shinythemes::themeSelector(),
  column(10, offset = 4, titlePanel("DynastyProcess.com Trade Calculator")),
  hr(),
  fluidRow(column(4,
                  sliderInput("slider1", "Depth Weight", min = -0.03,
                              max = -.02, value = -.024, step = 0.001)),
           column(4,
                  radioButtons("numQB", "Choose League Type",
                                     choices = list("1QB" = "dynoECR", "2QB / Superflex" = "dyno2QBECR"), 
                                     selected = "dynoECR"))),
  hr(),
  fluidRow(column(6,
                  selectizeInput("sideA",
                                 "Choose Assets to Team A:",
                                 choices = x["Name"],
                                 multiple = TRUE)),
           column(6,
                  selectizeInput("sideB",
                                 "Choose Assets to Team B:",
                                 choices = x["Name"],
                                 multiple = TRUE))),
  hr(),
  column(10, offset = 4, h3(textOutput("winner"))),
  column(10, offset = 4, h3(textOutput("winRange"))),
  fluidRow(column(6,
                  h4(textOutput("textA")),
                  tableOutput("tableA")),
           column(6,
                  h4(textOutput("textB")),
                  tableOutput("tableB"))),
  hr(),
  fluidRow(column(6,
                  plotOutput("bar")),
           column(6,
                  h4(textOutput("tableText")),
                  tableOutput("diffTable")))
  
))
