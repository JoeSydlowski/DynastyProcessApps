library(shiny)
library(curl)
library(DT)
library(shinythemes)
library(dplyr)
library(tibble)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols <- c(15,18:27,30:80)
y <- x[cols]

cols2 <- c(28:51, 53:62)
y$draft_round[is.na(y$draft_round)] <- 8
y[cols2][is.na(y[cols2])] <- 0

cols3 <- c(18,30:33)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  withTags(
    nav(class="navbar navbar-default navbar-static-top", role="navigation",
        div(class="container-fluid",
            div(class="navbar-header",
                span(class="navbar-brand",
                     a(href="https://dynastyprocess.com",strong("DynastyProcess.com"))
                )
            ),
            ul(class="nav navbar-nav",
               li(a(href="http://apps.dynastyprocess.com/database",strong("Database"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/calculator",strong("Calculator"))
               ),
               li(class="dropdown",
                  a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                  ul(class="dropdown-menu",
                     li(class="active",a(href="#",strong("Arbitrage"))),
                     li(a(href="http://apps.dynastyprocess.com/ecr",strong("ECR Explorer"))),
                     li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                  )
               )
            )
        )
    )
  ),
titlePanel("DynastyProcess.com Arbitrage App"),
  hr(),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["mergename"],
                 multiple = FALSE,
                 selected = "Tyler Boyd"),
  hr(),
  fluidRow(
    column(3,
           selectizeInput("selectcol",
                          "Select Comparison Variables:",
                          choices = list(
                            bio = names(x)[cols3],
                            "season-stats-fantasy" = names(x)[79:80],
                            "season-stats-snaps" = names(x)[47:49],
                            "season-stats-passing" = names(x)[50:56],
                            "season-stats-rushing" = names(x)[57:62],
                            "season-stats-receiving" = names(x)[63:75],
                            "season-stats-firstdowns" = names(x)[76:78],
                            athletic = names(x)[34:46],
                            contract = names(x)[19:20],
                            fantasypros = names(x)[21:27]),
                          multiple = TRUE)),
    column(3,
           radioButtons("numcomps", "Number of Comps",
                        choices = list("5" = 6, "10" = 11), 
                        selected = 6)),
    column(3,
           selectInput("presets", "Select a Recommended Set of Variables", 
                       choices = list("Default" = 0, "QB" = 1, "RB Opportunity" = 2,
                                      "RB Efficiency" = 3, "RB Athletic" = 4,
                                      "WR Opportunity" = 5, "WR Efficiency" = 6,
                                      "WR Athletic" = 7, "TE Opportunity" = 8,
                                      "TE Efficiency" = 9, "TE Athletic" = 10))),
    column(3,
           checkboxGroupInput("posFilter", "Positional Comps", 
                              choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE"),
                              selected = c("QB","RB","WR","TE")))),
  hr(),
  uiOutput("intro"),
  hr(),
  textOutput("sampleSize"),
  hr(),
  DTOutput("results"),
  hr(),
  p(HTML(paste0("This app was created by ",
                a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
                " based on data from ",
                a(href = "https://dynastyprocess.com/downloads/database/", "DynastyProcess.com"),
                ". You can find the code at ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/arbitrage", "my github"),
                "."
                )))
))
