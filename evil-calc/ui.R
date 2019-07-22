library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(rvest)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-players.csv"))

webpage <- read_html('https://github.com/tanho63/dynastyprocess/blob/master/files/values.csv')
lastupdate <- webpage %>%
  html_nodes("relative-time") %>%
  html_text()

cols <- c(1:6)

x <- x[cols]
names(x)[1]<-"Name"
x$dyno2QBECR[is.na(x$dyno2QBECR)] <- 400

shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  #shinythemes::themeSelector(),
  withTags(
    nav(class="navbar navbar-default navbar-static-top", role="navigation",
        div(class="container-fluid",
            div(class="navbar-header",
                span(class="navbar-brand",
                     a(href="https://dynastyprocess.com",strong("DynastyProcess.com"))
                ),
                button(type="button", class="navbar-toggle", `data-toggle`="collapse", `data-target`="#myNavbar",
                       span(class="icon-bar"),
                       span(class="icon-bar"),
                       span(class="icon-bar")
                )
            ),
            div(class="collapse navbar-collapse", id="myNavbar",
                ul(class="nav navbar-nav",
                   li(a(href="http://apps.dynastyprocess.com/database",strong("Database"))
                   ),
                   li(class="dropdown active",
                      a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="Calculator",`aria-expanded`="false", href="https://apps.dynastyprocess.com/calculator", strong("Calculator"),b(class="caret")),
                      ul(class="dropdown-menu",
                         li(a(href="https://apps.dynastyprocess.com/calculator",strong("Normal Mode"))),
                         li(class="active",a(href="#",strong("Dark Mode")))
                      )
                   ),
                   li(
                     a(href="http://apps.dynastyprocess.com/rookie-adp",strong("Rookie ADP"))
                   ),
                   li(class="dropdown",
                      a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                      ul(class="dropdown-menu",
                         li(a(href="http://apps.dynastyprocess.com/arbitrage",strong("Arbitrage"))),
                         li(a(href="http://apps.dynastyprocess.com/ecr",strong("ECR Explorer"))),
                         li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                      )
                   )
                ))
        )
    )
  ),
titlePanel("DynastyProcess.com Trade Calculator"),
  hr(),
  p(HTML(paste0("DynastyProcess.com is solar-powered and recommends ",a(href="https://dynastyprocess.com/calculator","Light Mode"), ". Proceed at your own risk."))),
  hr(),
  fluidRow(column(4,
                  radioButtons("numQB", "Choose League Type",
                               choices = list("1QB" = "dynoECR", "2QB / Superflex" = "dyno2QBECR"),
                               inline=TRUE,
                               selected = "dynoECR")),
           column(4,
                  radioButtons("calcType", "Startup Mode",
                               inline=TRUE,
                               choices = list("Normal" = "normal",
                                              "Startup (Players and 2019 Picks)" = "predraft",
                                              "Startup (Players Only)" = "postdraft"),
                               selected= "normal")),
           column(4,
                  selectInput("leagueSize", "Number of Teams",
                              choices = list("8" = 8,
                                             "10" = 10,
                                             "12" = 12,
                                             "14" = 14,
                                             "16" = 16,
                                             "18" = 18,
                                             "20" = 20),
                              selected = 12))),
  fluidRow(         
           column(4, 
                  sliderInput("slider1", "Depth Weight", min = -0.03,
                              max = -.02, value = -.024, step = 0.001,
                              label = div(style='width: 500px ; max-width: 100% ;' , 
                                          div(style='float:left;', 'Studs-Heavy'), 
                                          div(style='float:right;', 'Depth-Heavy')),
                              width= '500px'
                  )),
           column(4,
                  sliderInput("slider2", "Pick Value", min = 0,
                              max = 1, value = 0.5, step = 0.1,
                              label = div(style='width: 500px ; max-width: 100% ;' , 
                                          div(style='float:left;', 'Pick Skeptic'), 
                                          div(style='float:right;', 'Pick Optimist')),
                              width= '500px'
                  )),
           column(4,
                  downloadButton("downloadData", "Download Values"))),
  hr(),
  fluidRow(column(6,
                  selectizeInput("sideA",
                                 "Team A:",
                                 choices = x["Name"],
                                 multiple = TRUE,
                                 width='80%')),
           column(6,
                  selectizeInput("sideB",
                                 "Team B:",
                                 choices = x["Name"],
                                 width='80%',
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
                  tableOutput("diffTable"))),
  hr(),
  p(paste0("Player values last updated on ", lastupdate, ".")),
p(HTML(paste0("DynastyProcess.com Apps are created by ",
              a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
              " and ",
              a(href = "https://twitter.com/_TanHo", "Tan Ho"),
              ". You can find the code on ",
              a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/database", "Joe's github"),
              "."
)))
))
