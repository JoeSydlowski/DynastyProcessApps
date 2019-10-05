library(dplyr)
library(nflscrapR)

setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')
load("models.rda")
#setwd("/srv/shiny-server/DynastyProcess/ep")

ids <- scrape_game_ids(2019, type = "reg", weeks = c(1:5)) #%>%

ids$game_id <- as.character(ids$game_id)
id <- ids %>% pull(game_id)

dfnew <-  data.frame()
for (i in id)
{ print(i)
  tryCatch({df <- scrape_json_play_by_play(i)
  dfnew <- bind_rows(dfnew,df)}, error=function(e){print(paste("Game", i, "Unavailable"))})
}

dfnew <- dfnew %>%
  inner_join(dplyr::select(ids, game_id, week), by = c("game_id"="game_id"))

dfnew$posteam <- as.character(dfnew$posteam)
dfnew$td_team <- as.character(dfnew$td_team)

#Split the rush data and aggregate by week
rushdf <- dfnew %>% 
  filter(play_type %in% c("run"),
         two_point_attempt == 0) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         RushFP = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
         RushFP1D = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + 0.5*first_down_rush,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap))
  )

rushdf$eRushTD <- plogis(predict(rushTDMod, rushdf))
rushdf$eRushYD <- predict(rushYDMod, rushdf)
rushdf$eRush1D <- plogis(predict(rush1DMod, rushdf))
rushdf$eRushFP <- predict(rushFPMod, rushdf)
rushdf$eRushFP1D <- predict(rushFP1DMod, rushdf)

weeklyTeamRushDF <- rushdf %>%
  group_by(posteam, week) %>%
  summarise(TeamRushes = sum(rush_attempt, na.rm = TRUE),
            TeamRushYD = sum(yards_gained, na.rm = TRUE),
            TeamRush1D = sum(first_down_rush, na.rm = TRUE),
            TeamRushTD = sum(rush_touchdown, na.rm = TRUE),
            eTeamRushFP = sum(eRushFP, na.rm = TRUE),
            TeamRushFP = sum(RushFP, na.rm = TRUE),
            eTeamRushFP1D = sum(eRushFP1D, na.rm= TRUE),
            TeamRushFP1D = sum(RushFP1D, na.rm = TRUE)
            ) %>%
  ungroup()

weeklyRushDF <- rushdf %>%
  group_by(rusher_player_id, posteam, week) %>%
  summarise(Rushes = sum(rush_attempt, na.rm = TRUE),
            RushYD = sum(yards_gained, na.rm = TRUE),
            Rush1D = sum(first_down_rush, na.rm = TRUE),
            RushTD = sum(rush_touchdown, na.rm = TRUE),
            eRushFP = sum(eRushFP, na.rm = TRUE),
            RushFP = sum(RushFP, na.rm = TRUE),
            RushDiff = (RushFP - eRushFP),            
            eRushFP1D = sum(eRushFP1D, na.rm= TRUE),
            RushFP1D = sum(RushFP1D, na.rm = TRUE),
            RushDiff1D = (RushFP1D - eRushFP1D),
            RushGames = n_distinct(game_id)) %>%
  ungroup() %>%
  inner_join(weeklyTeamRushDF, by = c("posteam"="posteam", "week"="week"))

#Split the rec data and aggregate by week
recdf <- dfnew %>% 
  filter(play_type %in% c("pass"),
         sack == 0,
         two_point_attempt == 0) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         RecFP = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         RecFP1D = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass+ 0.5*first_down_pass,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         abs_air_yards = abs(air_yards)
         )

recdf$eRecTD <- plogis(predict(recTDMod, recdf))
recdf$eRecYD <- predict(recYDMod, recdf)
recdf$eRec <- plogis(predict(recMod, recdf))
recdf$eRec1D <- plogis(predict(rec1DMod, recdf))
recdf$eRecFP <- predict(recFPMod, recdf)
recdf$eRecFP1D <- predict(recFP1DMod, recdf)

weeklyTeamRecDF <- recdf %>%
  group_by(posteam, week) %>%
  summarise(TeamTar = sum(pass_attempt, na.rm = TRUE),
            TeamRec = sum(complete_pass, na.rm = TRUE),
            TeamAYs = sum(abs_air_yards, na.rm = TRUE),
            TeamRecYD = sum(yards_gained, na.rm = TRUE),
            TeamRec1D = sum(first_down_pass, na.rm = TRUE),
            TeamRecTD = sum(pass_touchdown, na.rm = TRUE),
            eTeamRecFP = sum(eRecFP, na.rm = TRUE),
            TeamRecFP = sum(RecFP, na.rm = TRUE),
            eTeamRecFP1D = sum(eRecFP1D, na.rm= TRUE),
            TeamRecFP1D = sum(RecFP1D, na.rm = TRUE)
            ) %>%
  ungroup()

weeklyRecDF <- recdf %>%
  group_by(receiver_player_id, posteam, week) %>%
  summarise(Tar = sum(pass_attempt, na.rm = TRUE),
            Rec = sum(complete_pass, na.rm = TRUE),
            AYs = sum(abs_air_yards, na.rm=TRUE),
            RecYD = sum(yards_gained, na.rm = TRUE),
            Rec1D = sum(first_down_pass, na.rm = TRUE),
            RecTD = sum(pass_touchdown, na.rm = TRUE),
            eRecFP = sum(eRecFP, na.rm = TRUE),
            RecFP = sum(RecFP, na.rm = TRUE),
            RecDiff = (RecFP - eRecFP),            
            eRecFP1D = sum(eRecFP1D, na.rm= TRUE),
            RecFP1D = sum(RecFP1D, na.rm = TRUE),
            RecDiff1D = (RecFP1D - eRecFP1D),
            RecGames = n_distinct(game_id)) %>%
  ungroup() %>%
  inner_join(weeklyTeamRecDF, by = c("posteam"="posteam", "week"="week"))

dfnewmerged <- bind_rows(weeklyRushDF, weeklyRecDF)

# dfnewmerged$play_id <- as.numeric(dfnewmerged$play_id)
# 
# df2019 <- read.csv("data2019cleaned.csv")
# df2019$X <- NULL
# 
# finaldf <- rbind(df2019, dfnewmerged) %>%
#  distinct()

write.csv(dfnewmerged, file = "data2019cleaned2.csv")
