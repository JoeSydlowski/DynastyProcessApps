library(shiny)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')

# ids <- scrape_game_ids(2019, type = "reg", weeks = c(2:2)) %>%
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
# save(df2019, file = "data19.rda")

load(file = "data19.rda")

rushdf2019 <- df2019 %>% 
    filter(!is.na(epa),
           play_type %in% c("run")) %>%
    mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
           posTD = if_else(posteam == td_team, touchdown, 0, missing = 0),
           rushFP = 6*posTD + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
           logyardline = log(yardline_100),
           yardlinesq = yardline_100*yardline_100,
           remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
           logremaining = log(remaining),
           run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap)))

recdf2019 <- df2019 %>%
    filter(!is.na(epa),
           play_type %in% c("pass")) %>%
    mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
           posTD = if_else(posteam == td_team, touchdown, 0, missing = 0),
           recFP = 6*posTD + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
           logyardline = log(yardline_100),
           yardlinesq = yardline_100*yardline_100,
           remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
           logremaining = log(remaining),
           abs_air_yards = abs(air_yards)
    )

load(file = "models.rda")

recdf2019$recEP <- predict(recMod, recdf2019)
rushdf2019$rushEP <- predict(rushMod, rushdf2019)

teamrecEP <- recdf2019 %>%
    group_by(posteam) %>%
    summarise(TeamTargets = sum(pass_attempt),
              TeamCatches = sum(complete_pass),
              TeamAYs = sum(abs_air_yards, na.rm=TRUE),
              TeamRecEP = sum(recEP, na.rm=TRUE)
    )

playerrushEP <- rushdf2019 %>%
    group_by(rusher_player_name, posteam) %>%
    summarise(rushEP = sum(rushEP, na.rm=TRUE),
              rushFP = format(sum(rushFP), scientific = FALSE),
              diff = format(as.numeric(rushFP) - rushEP, nsmall=1),
              Rushes = sum(rush_attempt))

playerrecEP <- recdf2019 %>%
    filter(!is.na(receiver_player_name)) %>%
    group_by(receiver_player_name, posteam) %>%
    summarise(recEP = sum(recEP, na.rm=TRUE),
              recFP = format(sum(recFP), scientific = FALSE),
              diff = format(as.numeric(recFP) - recEP, nsmall=1),
              Targets = sum(pass_attempt),
              Catches = sum(complete_pass),
              AYs = sum(air_yards),
              recYds = sum(yards_gained),
              aDOT = mean(air_yards)
    ) %>%
    inner_join(teamrecEP, by = "posteam") %>%
    mutate(AYshare = AYs / TeamAYs,
           TargetShare = Targets / TeamTargets,
           WOPR = 1.4*TargetShare + 0.7*AYshare,
           RACR = recYds / AYs,
           YPTPA = recYds / TeamTargets) %>%
    dplyr::select(receiver_player_name, posteam, recEP, recFP, diff, Targets, Catches, recYds, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)

#playerdf <- bind_rows(playerrecEP, playerrushEP) %>%
#    mutate(
#        Name = coalesce(receiver_player_name, rusher_player_name)
#    )
playerdf <- playerrecEP %>%
    full_join(playerrushEP, by = c("receiver_player_name" = "rusher_player_name", "posteam" = "posteam"))


shinyServer(function(input, output) {
    output$teamTable <- renderDT({
        df <- playerdf %>%
            filter(posteam == input$selectTeam)
        
        datatable(df,
                  rownames=FALSE,
                  options(
                      paging=FALSE,
                      searching=FALSE)) %>%
            formatRound(columns=c(3:18), digits=1)
        
    })
    
    
})
