library(shiny)
library(curl)
library(shinythemes)
library(DT)
library(rvest)
library(shinyWidgets)

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
  theme = shinytheme("flatly"),
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
                         li(class="active",a(href="#",strong("Normal Mode"))),
                         li(a(href="https://apps.dynastyprocess.com/evil-calc",strong("Dark Mode")))
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
  div(
    hr(),
    fluidRow(
    column(1,h3("About", style="text-align: center")),
    column(10, includeMarkdown("about.md"))
  ),class="hidden-xs"),
  hr(),
  fluidRow(div(class="text-center",column(2,
                  radioGroupButtons(
                    inputId = "calcType",
                    label = NULL,
                    choices = list("Normal" = "normal",
                                   "Startup Mode (Players and Picks)" = "predraft",
                                   "Startup Mode (Players Only)" = "postdraft"),
                    direction = "vertical",
                    status ="success"
                  ))
                  
  ),
  column(2,div(class="text-center",
         switchInput(
           inputId = "numQB",
           label = "Mode",
           onLabel = "1QB",
           offLabel = "2QB",
           offStatus = "warning",
           size = "large",
           value = TRUE,
           labelWidth = "100px",
           width = "100%"
         ),
         selectInput("leagueSize", NULL,
                     choices = list("8 Teams" = 8,
                                    "10 Teams" = 10,
                                    "12 Teams" = 12,
                                    "14 Teams" = 14,
                                    "16 Teams" = 16,
                                    "18 Teams" = 18,
                                    "20 Teams" = 20),
                     selected = 12,
                     width = '100%'
         ),
         downloadButton("downloadData", "Download Values", class="btn radiobtn btn-primary btn-block")
         )),
  column(4,
         br(),
         sliderInput("slider1", "Depth Weight", min = -0.03,
                     max = -.02, value = -.024, step = 0.001,
                     label = div(style='width: 800px ; max-width: 100% ;' , 
                                 div(style='float:left;', 'Studs-Heavy'), 
                                 div(style='float:right;', 'Depth-Heavy')),
                     width= '100%'
         )
  ),
  column(4,
         br(),
         sliderInput("slider2", "Pick Value", min = 0,
                     max = 1, value = 0.5, step = 0.1,
                     label = div(style='width: 800px ; max-width: 100% ;' , 
                                 div(style='float:left;', 'Pick Skeptic'), 
                                 div(style='float:right;', 'Pick Optimist')),
                     width= '100%')
  )
  ),
  hr(),
  fluidRow(column(6,
                  selectizeInput("sideA",
                                 "Team A:",
                                 choices = x["Name"],
                                 width='90%',
                                 multiple = TRUE)),
           column(6,
                  selectizeInput("sideB",
                                 "Team B:",
                                 choices = x["Name"],
                                 width='90%',
                                 multiple = TRUE))),
  hr(),
  column(10, offset=1, div(class="text-center",h3(textOutput("winner")))),
  column(10, offset = 1, div(class="text-center",h3(textOutput("winRange")))),
  fluidRow(column(6,
                  h4(textOutput("textA"),class="text-center"),
                  tableOutput("tableA")),
           column(6,
                  h4(textOutput("textB"),class="text-center"),
                  tableOutput("tableB"))),
  hr(),
  fluidRow(column(6,div(class="text-center",
                  plotOutput("bar"))),
           column(6,
                  h4(textOutput("tableText")),
                  tableOutput("diffTable"))),
  hr(),
  div(fluidRow(
    column(1,h3("About", style="text-align: center")),
    column(10, includeMarkdown("about.md")),
    hr()
  ),class="visible-xs",id="About"),
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
