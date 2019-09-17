library(shiny)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)

setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')
#setwd("/srv/shiny-server/DynastyProcess/ep")

# ids <- scrape_game_ids(2019, type = "reg", weeks = c(1:2)) %>%
#     filter(state_of_game == "POST")
# 
# ids$game_id <- as.character(ids$game_id)
# id <- ids %>% pull(game_id)
# 
# df2019 <-  data.frame()
# pbp_data <- for (i in id)
# {df <- scrape_json_play_by_play(i)
# df2019 <- bind_rows(df2019,df)}
# 
# df2019 <- df2019 %>%
#     inner_join(dplyr::select(ids, game_id, week), by = c("game_id"="game_id"))
# write.csv(df2019, file = "data2019.csv")

df2019 <- read.csv("data2019.csv")

shinyServer(function(input, output, server) {
    df <- reactive({
        
        df2019$posteam <- as.character(df2019$posteam)
        df2019$td_team <- as.character(df2019$td_team)
        
        rushdf2019 <- df2019 %>% 
            filter(!is.na(epa),
                   play_type %in% c("run")) %>%
            mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
                   posTD = if_else(posteam == td_team & touchdown == 1, 1, 0, missing = 0),
                   rushFP = 6*posTD + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
                   logyardline = log(yardline_100),
                   yardlinesq = yardline_100*yardline_100,
                   game_seconds_remaining = as.numeric(game_seconds_remaining),
                   remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
                   logremaining = log(remaining),
                   run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap))
            )
        
        recdf2019 <- df2019 %>%
            filter(!is.na(epa),
                   play_type %in% c("pass"),
                   sack == 0) %>%
            mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
                   posTD = if_else(posteam == td_team & touchdown == 1, 1, 0, missing = 0),
                   recFP = 6*posTD + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
                   logyardline = log(yardline_100),
                   yardlinesq = yardline_100*yardline_100,
                   game_seconds_remaining = as.numeric(game_seconds_remaining),
                   remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
                   logremaining = log(remaining),
                   abs_air_yards = abs(air_yards)
            )
        
        #load("/srv/shiny-server/DynastyProcess/ep/models.rda")
        load("models.rda")
        
        recdf2019$recEP <- predict(recMod, recdf2019)
        rushdf2019$rushEP <- predict(rushMod, rushdf2019)

        teamrecEP <- recdf2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., posteam, week) else group_by(., posteam)} %>%
            summarise(TeamTargets = sum(pass_attempt),
                      TeamCatches = sum(complete_pass),
                      TeamAYs = sum(air_yards, na.rm=TRUE),
                      TeamRecEP = sum(recEP, na.rm=TRUE)
            )
        
        playerrushEP <- rushdf2019 %>%
            {if (input$weeklyRadio == "Weekly") group_by(., rusher_player_name, posteam, week) else group_by(., rusher_player_name, posteam)} %>%
            summarise(rushEP = sum(rushEP, na.rm=TRUE),
                      rushFP = sum(rushFP, na.rm=TRUE),
                      rushDiff = (rushFP - rushEP),
                      Rushes = sum(rush_attempt))

        playerrecEP <- recdf2019 %>%
            filter(!is.na(receiver_player_name)) %>%
            {if (input$weeklyRadio == "Weekly") group_by(., receiver_player_name, posteam, week) else group_by(., receiver_player_name, posteam)} %>%            
            summarise(recEP = sum(recEP, na.rm=TRUE),
                      recFP = sum(recFP, na.rm=TRUE),
                      recDiff = (recFP - recEP),
                      Targets = sum(pass_attempt),
                      Catches = sum(complete_pass),
                      AYs = sum(air_yards),
                      recYds = sum(yards_gained),
                      aDOT = mean(air_yards)
            ) %>%
            {if (input$weeklyRadio == "Weekly") inner_join(., teamrecEP, by = c("posteam"="posteam","week"="week")) else
                inner_join(., teamrecEP, by = c("posteam"="posteam")) } %>%
            mutate(AYshare = AYs / TeamAYs,
                   TargetShare = Targets / TeamTargets,
                   WOPR = 1.5*TargetShare + 0.7*AYshare,
                   RACR = recYds / AYs,
                   YPTPA = recYds / TeamTargets) %>%
            {if (input$weeklyRadio == "Weekly") dplyr::select(., week, receiver_player_name, posteam, recEP, recFP, recDiff, Targets, Catches, recYds, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA) else
                dplyr::select(., receiver_player_name, posteam, recEP, recFP, recDiff, Targets, Catches, recYds, AYs, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)}

        #playerdf <- bind_rows(playerrecEP, playerrushEP) %>%
        #    mutate(
        #        Name = coalesce(receiver_player_name, rusher_player_name)
        #    )
        playerdf <- playerrecEP %>%
            {if (input$weeklyRadio == "Weekly") full_join(., playerrushEP, by = c("receiver_player_name" = "rusher_player_name", "posteam" = "posteam", "week"="week")) else
                full_join(., playerrushEP, by = c("receiver_player_name" = "rusher_player_name", "posteam" = "posteam"))}
        
    })
    
    output$teamTable <- renderDT({
        df2 <- df() %>%
            {if (input$selectTeam != "All") filter(., posteam == input$selectTeam) else . }
        
        datatable(df2,
                  rownames=FALSE,
                  options(
                      paging=FALSE,
                      searching=FALSE)) #%>%
            #formatRound(columns=c((ncol(df2)-15):ncol(df2)), digits=1)
        
    })
    
    
})
