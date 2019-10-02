library(shiny)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
library(shinydashboard)
library(here)

database <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv", fileEncoding = "UTF-8-BOM")
database$gsis_id <- as.character(database$gsis_id)

setwd(here())
#setwd("C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep")

df2019 <- read.csv("data2019cleaned.csv")

shinyServer(function(input, output, session) {
    df <- reactive({
        
        teamrecEP <- df2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., posteam, week) else group_by(., posteam)} %>%
            summarise(TeamTargets = sum(pass_attempt),
                      TeamCatches = sum(complete_pass),
                      TeamAYs = sum(air_yards, na.rm=TRUE),
                      TeamRecEP = sum(eRecFP, na.rm=TRUE)
            ) %>%
            ungroup()
        
        playerrushEP <- df2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., rusher_player_id, posteam, week) else group_by(., rusher_player_id, posteam)} %>%
            summarise(eRushFP = sum(eRushFP, na.rm=TRUE),
                      RushFP = sum(RushFP, na.rm=TRUE),
                      RushDiff = (RushFP - eRushFP),
                      Rushes = sum(rush_attempt),
                      RushGames = n_distinct(game_id)) %>%
            ungroup()
        
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
            ungroup() %>%
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