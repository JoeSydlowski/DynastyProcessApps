library(shiny)
library(dplyr)
#library(nflscrapR)
library(DT)
library(tidyr)
library(curl)

database <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv", fileEncoding = "UTF-8-BOM")
database$gsis_id <- as.character(database$gsis_id)

setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')
#setwd("/srv/shiny-server/DynastyProcessApps/ep")

df2019 <- read.csv("data2019cleaned.csv")

shinyServer(function(input, output, session) {
    df <- reactive({
        
        teamrecEP <- df2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., posteam, week) else group_by(., posteam)} %>%
            summarise(TeamTargets = sum(pass_attempt),
                      TeamCatches = sum(complete_pass),
                      TeamAYs = sum(air_yards, na.rm=TRUE),
                      TeamRecEP = sum(eRecFP, na.rm=TRUE)
            )
        
        playerrushEP <- df2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., rusher_player_id, posteam, week) else group_by(., rusher_player_id, posteam)} %>%
            summarise(eRushFP = sum(eRushFP, na.rm=TRUE),
                      RushFP = sum(RushFP, na.rm=TRUE),
                      RushDiff = (RushFP - eRushFP),
                      Rushes = sum(rush_attempt),
                      RushGames = n_distinct(game_id))
        
        playerrecEP <- df2019 %>%
            #filter(!is.na(receiver_player_id)) %>%
            {if (input$weeklyRadio == "Weekly") group_by(., receiver_player_id, posteam, week) else group_by(., receiver_player_id, posteam)} %>%            
            summarise(eRecFP = sum(eRecFP, na.rm=TRUE),
                      RecFP = sum(RecFP, na.rm=TRUE),
                      RecDiff = (RecFP - eRecFP),
                      Targets = sum(pass_attempt),
                      Catches = sum(complete_pass),
                      AYs = sum(abs_air_yards),
                      RecYds = sum(yards_gained),
                      aDOT = mean(abs_air_yards),
                      RecGames = n_distinct(game_id)
            ) %>%
            {if (input$weeklyRadio == "Weekly") inner_join(., teamrecEP, by = c("posteam"="posteam","week"="week")) else
                inner_join(., teamrecEP, by = c("posteam"="posteam")) } %>%
            mutate(AYshare = AYs / TeamAYs,
                   TargetShare = Targets / TeamTargets,
                   WOPR = 1.5*TargetShare + 0.7*AYshare,
                   RACR = RecYds / AYs,
                   YPTPA = RecYds / TeamTargets) %>%
            {if (input$weeklyRadio == "Weekly") dplyr::select(., week, receiver_player_id, posteam, eRecFP, RecGames, RecFP, RecDiff, Targets, Catches, RecYds, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA) else
                dplyr::select(., receiver_player_id, posteam, eRecFP, RecGames, RecFP, RecDiff, Targets, Catches, RecYds, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)}
        
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
            {if (input$weeklyRadio == "Weekly") dplyr::select(., week, mergename, posteam, pos, eRecFP, RecFP, RecDiff, eRushFP, RushFP, eRushFP, RushDiff, eFP, FP, Diff) else
                dplyr::select(., mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, eRushFP, RushDiff, eFP, FP, Diff)}
    })
    
    df2 <- reactive({
        df() %>%
            {if (input$selectTeam != "All") filter(., posteam == input$selectTeam) else . } %>%
            {if (input$selectPos  != "All") filter(., pos == input$selectPos) else .} %>%
            {if (input$weeklyRadio == "Weekly") filter(., week %in% input$selectWeeks) else .}
    })
    
    # observeEvent({input$selectTeam
    #     input$selectPos},{
    #         
    #         currentPlayer <- input$selectPlayers
    #         updateSelectizeInput(session, 'selectPlayers',
    #                              choices = c("All", as.character(sort(unique(df2()$mergename)))),
    #                              selected = c(currentPlayer)
    #         )
    #     })
    
    output$teamTable <- renderDT({
        datatable(df2(),
                  rownames=FALSE,
                  options(
                      paging=FALSE,
                      searching=FALSE)) %>%
            #formatRound(columns=c((ncol(df2)-15):ncol(df2)), digits=1)
            formatRound(columns=c(6:ncol(df2())), digits=1)
        
    })
    
    
})