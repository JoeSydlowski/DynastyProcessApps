library(shiny)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
library(shinydashboard)
library(here)

setwd(here())

df2019 <- read.csv("data2019cleaned.csv")

shinyUI(
  dashboardPage(
    skin = "blue",
    title = "DynastyProcess Apps: Expected Points",
    dashboardHeader(
      title = a(href = "https://dynastyprocess.com", img(src = "logo-horizontal.png", width =
                                                           '100%')),
      titleWidth = 250
    ),
    dashboardSidebar(width = 250,
                     sidebarMenu(menuItem(
                       'EP', tabname = 'ep', icon = icon('rocket')
                     ))),
    dashboardBody(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/flatly.css"),
      tags$style(
        HTML(          '
                                /* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #000;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #555;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                  background-color: #000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                  background-color: #000;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #555;
                                  text-decoration: none;
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #555;
                                  text-decoration: none;
                                }
                                .skin-blue .sidebar-menu > li.active > a{
                                  border-left-color: #fff
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #fff;
                                }
                                .btn {
                                  font-size: 12px;
                                }

                                .selectize-input
                                {font-size:12px;
                                min-height:25px;
                                padding-top:0px;
                                padding-bottom:0px;
                                }

        '
        )
      )
    ),
    tabItems(
      tabItem(tabName = 'ep',
              fluidRow(
                box(
                  4,
                  selectInput(
                    "selectTeam",
                    "Select Team:",
                    choices = c("All", as.character(sort(
                      unique(df2019$posteam)
                    ))),
                    selected = "KC"
                  )
                ),
                box(
                  4,
                  selectInput(
                    "selectPos",
                    "Select Position:",
                    choices = c("All", "QB", "RB", "WR", "TE"),
                    selected = "All"
                  )
                ),
                box(
                  4,
                  radioButtons(
                    "weeklyRadio",
                    "Weekly or Cumulative?",
                    choices = c("Weekly", "Cumulative"),
                    selected = "Cumulative"
                  ),
                  conditionalPanel(
                    condition = "input.weeklyRadio == 'Weekly'",
                    selectizeInput(
                      "selectWeeks",
                      "Select Weeks:",
                      choices = sort(unique(df2019$week)),
                      selected = max(df2019$week),
                      multiple = TRUE
                    )
                  )
                )
              ),
              fluidRow(box(
                12,
                DTOutput("teamTable")
              )))
    ))
  )
  
  #
  #   fluidPage(
  #   selectInput("selectTeam",
  #               "Select Team:",
  #               choices = c("All", as.character(sort(unique(df2019$posteam)))),
  #               selected = "CHI"),
  #   selectInput("selectPos",
  #               "Select Position:",
  #               choices = c("All", "QB", "RB", "WR", "TE"),
  #               selected = "All"),
  #   radioButtons("weeklyRadio",
  #                "Weekly or Cumulative?",
  #                choices = c("Weekly","Cumulative"),
  #                selected = "Cumulative"),
  #   conditionalPanel(condition = "input.weeklyRadio == 'Weekly'",
  #                    selectizeInput("selectWeeks",
  #                                   "Select Weeks:",
  #                                   choices = sort(unique(df2019$week)),
  #                                   selected = max(df2019$week),
  #                                   multiple = TRUE)),
  #   # selectizeInput("selectPlayers",
  #   #                "Select Players:",
  #   #                choices = c("All"),
  #   #                selected = "All",
  #   #                multiple = TRUE),
  #   DTOutput("teamTable")
  # )
) #end of UI code