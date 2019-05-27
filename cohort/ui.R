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
               li(
                 a(href="http://apps.dynastyprocess.com/rookie-adp",strong("Rookie ADP"))
               ),
               li(class="dropdown",
                  a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                  ul(class="dropdown-menu",
                     li(a(href="http://apps.dynastyprocess.com/arbitrage",strong("Arbitrage"))),
                     li(a(href="http://apps.dynastyprocess.com/ecr",strong("ECR Explorer"))),
                     li(class="active",a(href="#",strong("Cohort")))
                  )
               )
            )
        )
    )
  ),
  column(10, offset = 4, titlePanel("DynastyProcess.com Cohort App")),
  hr(),
  selectizeInput("selected",
                 "Select Player:",
                 choices = x["Player"],
                 multiple = FALSE,
                 selected = "Patrick Mahomes"),
  hr(),
  DTOutput("results"),
  p(HTML(paste0("DynastyProcess.com Apps are created by ",
                a(href = "https://twitter.com/JoeSydlowskiFF", "Joe Sydlowski"),
                " and ",
                a(href = "https://twitter.com/_TanHo", "Tan Ho"),
                ". You can find the code on ",
                a(href = "https://github.com/JoeSydlowski/DynastyProcess/tree/master/database", "Joe's github"),
                "."
  )))
  
  
))
