library(shiny)
library(curl)
library(DT)
library(dplyr)
library(anytime)
library(shinythemes)

x <- read.csv(curl("https://raw.githubusercontent.com/JoeSydlowski/DynastyProcess/master/rookie-adp/adp-picks.csv"))
x$pick_timestamp <- ifelse(x$MFL_Sleeper == "Sleeper", x$pick_timestamp/1000, x$pick_timestamp)
x$date <- anydate(x$pick_timestamp)

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
               li(a(href = "http://apps.dynastyprocess.com/database",strong("Database"))
               ),
               li(
                 a(href="http://apps.dynastyprocess.com/calculator",strong("Calculator"))
               ),
               li(
                 class="active",a(href="#",strong("Rookie ADP"))
               ),
               li(class="dropdown",
                  a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                  ul(class="dropdown-menu",
                     li(a(href="http://apps.dynastyprocess.com/arbitrage",strong("Arbitrage"))),
                     li(a(href="http://apps.dynastyprocess.com/ecr",strong("ECR Explorer"))),
                     li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                  )
               )
            )
        )
    )
  ),
  
  titlePanel("r/DynastyFF Community Rookie ADP"),
  p("This is a rookie ADP compilation based on contributions from the r/DynastyFF community. If interested in adding your league, please submit it in the form below!"),
  tabsetPanel(
    tabPanel("ADP",
  fluidRow(
    column(4,
           radioButtons("mode",
                        "Select Mode",
                        choices = list("1QB Offense-Only" = "1QB_O",
                                       "2QB/SF Offense-Only" = "2QB_O",
                                       "1QB IDP" = "1QB_IDP",
                                       "2QB/SF IDP" = "2QB_IDP"),
                        selected = "1QB_O")),
    column(4,
           dateRangeInput("dateRange",
                          "Date Range",
                          start = min(x$date),
                          end = max(x$date),
                          min = min(x$date),
                          max = max(x$date))),
    column(4,
            actionButton("clear1", "Reset All Filters"),
            downloadButton("downloadData", "Download"))),
  hr(),
  DTOutput("results"),
  hr()
    ),
  tabPanel("Add Your LeagueID Here!",
           includeHTML("form.html")
  )),
  p(HTML(paste0("DynastyProcess.com Apps are created by ",
                a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
                " and ",
                a(href = "https://twitter.com/_TanHo", "Tan Ho"),
                ". You can find the code on ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/database", "Joe's github"),
                "."
  )))
))
