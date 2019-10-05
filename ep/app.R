library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
library(here)

database <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv", fileEncoding = "UTF-8-BOM")
database$gsis_id <- as.character(database$gsis_id)

setwd(here())
#setwd("C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep")

df2019 <- read.csv("data2019cleaned2.csv")

ui <- dashboardPage(skin="blue",
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
                                                  selected = "KC")
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
                                fluidRow(DTOutput("teamPivot")))
                      )
                    )
)
  
  # fluidPage(
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
  #   selectizeInput("selectPlayers",
  #                  "Select Players:",
  #                  choices = c("All"),
  #                  selected = "All",
  #                  multiple = TRUE),
  #   DTOutput("teamTable"),
  #   DTOutput("teamPivot")
  # )
#) #end of UI code

server <- shinyServer(function(input, output, session) {
  df <- reactive({
    
    # teamrecEP <- df2019 %>%
    #     {if (input$weeklyRadio == "Weekly") group_by(., posteam, week) else group_by(., posteam)} %>%
    #     summarise(TeamTargets = sum(pass_attempt),
    #               TeamCatches = sum(complete_pass),
    #               TeamAYs = sum(air_yards, na.rm=TRUE),
    #               TeamRecEP = sum(eRecFP, na.rm=TRUE)
    #     ) %>%
    #     ungroup()
    
    playerrushEP <- df2019 %>%
      {if (input$weeklyRadio == "Weekly") group_by(., rusher_player_id, posteam, week) else group_by(., rusher_player_id, posteam)} %>%
      summarise(eRushFP = sum(eRushFP, na.rm = TRUE),
                RushFP = sum(RushFP, na.rm = TRUE),
                RushDiff = sum(RushDiff, na.rm = TRUE),
                Rushes = sum(Rushes, na.rm = TRUE),
                RushGames = sum(RushGames, na.rm = TRUE)) %>%
      ungroup()
    
    playerrecEP <- df2019 %>%
      #filter(!is.na(receiver_player_id)) %>%
      {if (input$weeklyRadio == "Weekly") group_by(., receiver_player_id, posteam, week) else group_by(., receiver_player_id, posteam)} %>%            
      summarise(eRecFP = sum(eRecFP, na.rm=TRUE),
                RecFP = sum(RecFP, na.rm=TRUE),
                RecDiff = sum(RecDiff, na.rm = TRUE),
                Targets = sum(Tar, na.rm = TRUE),
                TeamTargets = sum(TeamTar, na.rm = TRUE),
                
                Catches = sum(Rec, na.rm = TRUE),
                AYs = sum(AYs, na.rm = TRUE),
                TeamAYs = sum(TeamAYs, na.rm = TRUE),
                
                RecYD = sum(RecYD, na.rm = TRUE),
                aDOT = mean(AYs, na.rm = TRUE),
                RecGames = sum(RecGames, na.rm = TRUE)) %>%
      # {if (input$weeklyRadio == "Weekly") inner_join(., teamrecEP, by = c("posteam"="posteam", "week"="week")) else
      #     inner_join(., teamrecEP, by = c("posteam"="posteam")) } %>%
      mutate(AYshare = AYs / TeamAYs,
             TargetShare = Targets / TeamTargets,
             WOPR = 1.5*TargetShare + 0.7*AYshare,
             RACR = RecYD / AYs,
             YPTPA = RecYD / TeamTargets) %>%
      ungroup() %>%
      {if (input$weeklyRadio == "Weekly") dplyr::select(., week, receiver_player_id, posteam, eRecFP, RecGames, RecFP, RecDiff, Targets, Catches, RecYD, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA) else
        dplyr::select(., receiver_player_id, posteam, eRecFP, RecGames, RecFP, RecDiff, Targets, Catches, RecYD, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)}
    
    playerdf <- playerrecEP %>%
      {if (input$weeklyRadio == "Weekly") full_join(., playerrushEP, by = c("receiver_player_id" = "rusher_player_id", "posteam" = "posteam", "week"="week")) else
        full_join(., playerrushEP, by = c("receiver_player_id" = "rusher_player_id", "posteam" = "posteam"))} %>%
      inner_join(database, by = c("receiver_player_id" = "gsis_id")) %>%
      {if (input$weeklyRadio == "Weekly") group_by(., receiver_player_id, posteam, week) else
        group_by(., receiver_player_id, posteam) } %>%
      mutate(
        eFP = sum(eRecFP, eRushFP, na.rm=TRUE),
        FP = sum(RecFP, RushFP, na.rm=TRUE),
        Diff = sum(FP, - eFP, na.rm = TRUE),
        Games = max(RushGames, RecGames, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      {if (input$weeklyRadio == "Weekly") dplyr::select(., week, mergename, posteam, pos, eRecFP, RecFP, RecDiff, eRushFP, RushFP, eRushFP, RushDiff, eFP, FP, Diff) else
        dplyr::select(., mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, eRushFP, RushDiff, eFP, FP, Diff)}
    
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
      select(mergename, week, eFP) %>%
      arrange(week) %>%
      pivot_wider(names_from = week,
                  names_prefix = "Week",
                  values_from = eFP) %>%
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
    datatable(df3(),
              rownames=FALSE,
              options(
                paging=FALSE,
                searching=FALSE)) %>%
      #formatRound(columns=c((ncol(df2)-15):ncol(df2)), digits=1)
      formatRound(columns=c(5:ncol(df3())), digits=1)
    
  })
  
  output$teamPivot <- renderDT({
    req(input$weeklyRadio == "Weekly")
    datatable(df4(),
              rownames=FALSE,
              options(
                paging=FALSE,
                searching=FALSE)) %>%
      formatRound(columns = c(2:ncol(df4())), digits = 1)
  })
})

shinyApp(ui, server)