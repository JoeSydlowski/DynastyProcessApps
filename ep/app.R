library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
library(here)
library(shinyWidgets)
library(ggplot2)

#setwd(here())
#setwd("C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep")
setwd("/srv/shiny-server/DynastyProcessApps/ep")

df2019 <- read.csv("data2019cleaned2.csv")

ui <- dashboardPage(
  skin="blue",
  title="DynastyProcess Apps: Expected Points",
  dashboardHeader(title = a(href="https://dynastyprocess.com",
                            img(src = "logo-horizontal.png",
                                width='100%')),
                  titleWidth = 250),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem('EP', tabName = 'ep', icon = icon('rocket')),
                     menuItem('EP2', tabName = 'ep2', icon = icon('quidditch')))
  ),
  dashboardBody(
    {tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/flatly.css"),
      tags$style(HTML('
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
                                      ')
      )
    )},
    tabItems(
      tabItem(tabName = 'ep',
              titlePanel('DynastyProcess Apps: Expected Points'),
              # fluidRow(includeMarkdown('about.md')),
              fluidRow(
                box(width = 3,
                    selectInput("selectTeam",
                                "Select Team:",
                                choices = c("All", as.character(sort(unique(df2019$posteam)))),
                                selected = "KC"),
                    radioGroupButtons("selectCol","Select Columns:", choices = c("Exp Points","Raw Stats","Rate Stats"),
                                      selected = "Exp Points")
                ),
                box(width = 3,
                    selectInput("selectPos",
                                "Select Position:",
                                choices = c("All", "QB", "RB", "WR", "TE"),
                                selected = "All")
                ),
                box(width = 3,
                    radioButtons("weeklyRadio",
                                 "Weekly or Cumulative?",
                                 choices = c("Weekly","Cumulative"),
                                 selected = "Cumulative"),
                    conditionalPanel(condition = "input.weeklyRadio == 'Weekly'",
                                     selectizeInput("selectWeeks",
                                                    "Select Weeks:",
                                                    choices = sort(unique(df2019$week)),
                                                    selected = max(df2019$week),
                                                    multiple = TRUE))),
                box(width = 3,
                    selectizeInput("selectPlayers",
                                   "Select Players:",
                                   choices = c("All"),
                                   selected = "All",
                                   multiple = TRUE)
                )),
              fluidRow(box(width = 12,
                           DTOutput("teamTable")))
      ),
      tabItem(tabName = "ep2",
              titlePanel('DynastyProcess Apps: Expected Points'),
              #fluidRow(includeMarkdown('about2.md')),
              fluidRow(box(width = 3,
                           selectInput("selectVar",
                                       "Select Variable:",
                                       choices = c("eRushFP","eTeamRushFP","RushFP","TeamRushFP","RushDiff","Rushes","RushGames",
                                                   "RushYD","eRecFP","eTeamRecFP","RecFP","TeamRecFP","RecDiff","Targets","TeamTargets","Catches","AYs",
                                                   "TeamAYs","RecYD","aDOT","RecGames","RushTD","RecTD","TD","eFP","FP","Diff",     
                                                   "eTeamFP","TeamFP","TeamDiff","AYshare","TargetShare","WOPR","RACR","YPTPA","eFPshare","FPshare"),
                                       selected = "eFP"))),
              fluidRow(box(width = 12,
                           plotOutput("pivotGraph"),
                           DTOutput("teamPivot"))))
    )
  )
)

server <- shinyServer(function(input, output, session) {
  df <- reactive({
    df2019 %>%
      {if (input$weeklyRadio == "Weekly") group_by(., mergename, pos, posteam, week) else group_by(., mergename, pos, posteam)} %>%
      summarise(eRushFP = sum(eRushFP, na.rm = TRUE),
                eTeamRushFP = sum(eTeamRushFP, na.rm = TRUE),
                RushFP = sum(RushFP, na.rm = TRUE),
                TeamRushFP = sum(TeamRushFP, na.rm = TRUE),
                
                RushDiff = sum(RushDiff, na.rm = TRUE),
                Rushes = sum(Rushes, na.rm = TRUE),
                RushGames = sum(RushGames, na.rm = TRUE),
                
                RushYD = sum(RushYD, na.rm = TRUE),
                
                eRecFP = sum(eRecFP, na.rm=TRUE),
                eTeamRecFP = sum(eTeamRecFP, na.rm=TRUE),
                RecFP = sum(RecFP, na.rm=TRUE),
                TeamRecFP = sum(TeamRecFP, na.rm = TRUE),
                RecDiff = sum(RecDiff, na.rm = TRUE),
                Targets = sum(Tar, na.rm = TRUE),
                TeamTargets = sum(TeamTar, na.rm = TRUE),
                
                Catches = sum(Rec, na.rm = TRUE),
                AYs = sum(AYs, na.rm = TRUE),
                TeamAYs = sum(TeamAYs, na.rm = TRUE),
                
                RecYD = sum(RecYD, na.rm = TRUE),
                aDOT = mean(AYs, na.rm = TRUE),
                RecGames = sum(RecGames, na.rm = TRUE),
                
                # eRushTD = sum(eRushTD, na.rm = TRUE),
                RushTD = sum(RushTD, na.rm = TRUE),
                # RushTDDiff = RushTD - eRushTD,
                # 
                # eRecTD = sum(eRecTD, na.rm = TRUE),
                RecTD = sum(RecTD, na.rm = TRUE),
                # RecTDDiff = RecTD - eRecTD,
                # 
                # eTD = eRushTD + eRecTD,
                TD = RushTD + RecTD,
                # TDDiff = TD - eTD,
                
                eFP = eRecFP + eRushFP,
                FP = RecFP + RushFP,
                Diff = FP - eFP,
                #eFP1D = eRecFP1D + eRushFP1D,
                #FP1D = RecFP1D + RushFP1D,
                #Diff1D = FP1D - eFP1D,
                eTeamFP = eTeamRecFP + eTeamRushFP,
                TeamFP = TeamRecFP + TeamRushFP,
                TeamDiff = TeamFP - eTeamFP,
                #eTeamFP1D = eTeamRecFP1D + eTeamRushFP1D,
                #TeamFP1D = TeamRecFP1D + TeamRushFP1D,
                #TeamDiff1D = TeamFP1D - eTeamFP1D
                AYshare = AYs / TeamAYs,
                TargetShare = Targets / TeamTargets,
                WOPR = 1.5*TargetShare + 0.7*AYshare,
                RACR = RecYD / AYs,
                YPTPA = RecYD / TeamTargets,
                eFPshare = eFP / eTeamFP,
                FPshare = FP/ TeamFP,
                Games = sum(Games, na.rm = TRUE)
                ) %>%
      ungroup()

  })
  
  df2 <- reactive({
    df() %>%
      {if (input$selectTeam != "All") filter(., posteam == input$selectTeam) else . } %>%
      {if (input$selectPos  != "All") filter(., pos == input$selectPos) else .} %>%
      {if (input$weeklyRadio == "Weekly") filter(., week %in% input$selectWeeks) else .} 
  })
  
  df3 <- reactive({
    df2() %>%
      {if (input$selectPlayers != "All") filter(., mergename %in% input$selectPlayers) else .}
  })
  
  df4 <- reactive({
    df3() %>%
      {if (input$selectCol == "Exp Points" & input$weeklyRadio == "Weekly")
        dplyr::select(., week, mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, RushDiff, eFP, FP, Diff)
        else if (input$selectCol == "Raw Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, Games, Rushes, RushYD, RushTD, Targets, Catches, AYs, RecYD, RecTD)
        else if (input$selectCol == "Rate Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, AYshare, TargetShare, WOPR, RACR, YPTPA, eFPshare, FPshare)
        else if (input$selectCol == "Exp Points")
          dplyr::select(., mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, RushDiff, eFP, FP, Diff)
        else if (input$selectCol == "Raw Stats")
          dplyr::select(., mergename, posteam, pos, Games, Rushes, RushYD, RushTD, Targets, Catches, AYs, RecYD, RecTD)
        else if (input$selectCol == "Rate Stats")
          dplyr::select(., mergename, posteam, pos, AYshare, TargetShare, WOPR, RACR, YPTPA, eFPshare, FPshare)
      }
    
  })
  
  df5 <- reactive({
    df3() %>%
      select(mergename, week, input$selectVar) %>%
      arrange(week) %>%
      pivot_wider(names_from = week,
                  names_prefix = "Week",
                  values_from = input$selectVar) %>%
      mutate(Total = rowSums(.[2:ncol(.)], na.rm = TRUE))
    
  })
  
  observeEvent({input$selectTeam
    input$selectPos
    input$selectPlayers},{
      currentPlayer <- input$selectPlayers
      if (currentPlayer[1] == "All" & length(currentPlayer) > 1)
      {currentPlayer <- currentPlayer[!currentPlayer %in% "All"]
      print(currentPlayer)}
      if("All" %in% currentPlayer & currentPlayer[1] != "All")
      {currentPlayer <- c("All")}
      
      updateSelectizeInput(session, 'selectPlayers',
                           choices = c("All", as.character(sort(unique(df2()$mergename)))),
                           selected = currentPlayer
      )
    })
  
  output$teamTable <- renderDT({
    datatable(df4(),
              rownames=FALSE,
              options(
                paging=FALSE,
                searching=FALSE)) %>%
      #formatRound(columns=c((ncol(df2)-15):ncol(df2)), digits=1)
      formatRound(columns=c(4:ncol(df3())), digits=1)
  })
  
  output$pivotGraph <- renderPlot({
    plotdf <- df3() %>%
      filter(!is.na(input$selectVar))
    print(df3())
    #selVar <- input$selectVar
    ggplot(plotdf,
           aes_string(x = plotdf$week, y = input$selectVar, color = plotdf$mergename)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", fill = NA) +
      theme_bw()
  })
  
  output$teamPivot <- renderDT({
    req(input$weeklyRadio == "Weekly")
    datatable(df5(),
              rownames=FALSE,
              options(
                paging=FALSE,
                searching=FALSE)) %>%
      formatRound(columns = c(2:ncol(df4())), digits = 1)
  })
})

shinyApp(ui, server)