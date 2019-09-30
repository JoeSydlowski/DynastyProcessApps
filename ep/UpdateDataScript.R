library(dplyr)
library(nflscrapR)

setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')
#setwd("/srv/shiny-server/DynastyProcess/ep")

ids <- scrape_game_ids(2019, type = "reg", weeks = c(1:4)) #%>%
  #filter(state_of_game == "POST")

ids$game_id <- as.character(ids$game_id)
id <- ids %>% pull(game_id)

dfnew <-  data.frame()
for (i in id)
{ print(i)
  tryCatch({df <- scrape_json_play_by_play(i)
  dfnew <- bind_rows(dfnew,df)}, error=function(e){"Game Unavailable"})
}

dfnew <- dfnew %>%
  inner_join(dplyr::select(ids, game_id, week), by = c("game_id"="game_id"))

dfnew$posteam <- as.character(dfnew$posteam)
dfnew$td_team <- as.character(dfnew$td_team)

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

load("models.rda")

recdf$eRecTD <- plogis(predict(recTDMod, recdf))
recdf$eRecYD <- predict(recYDMod, recdf)
recdf$eRec <- plogis(predict(recMod, recdf))
recdf$eRec1D <- plogis(predict(rec1DMod, recdf))
recdf$eRecFP <- predict(recFPMod, recdf)
recdf$eRecFP1D <- predict(recFP1DMod, recdf)

rushdf$eRushTD <- plogis(predict(rushTDMod, rushdf))
rushdf$eRushYD <- predict(rushYDMod, rushdf)
rushdf$eRush1D <- plogis(predict(rush1DMod, rushdf))
rushdf$eRushFP <- predict(rushFPMod, rushdf)
rushdf$eRushFP1D <- predict(rushFP1DMod, rushdf)

dfnewmerged <- bind_rows(recdf, rushdf)

dfnewmerged$play_id <- as.numeric(dfnewmerged$play_id)

# df2019 <- read.csv("data2019cleaned.csv")
# df2019$X <- NULL
# 
# finaldf <- rbind(df2019, dfnewmerged) %>%
#  distinct()

write.csv(dfnewmerged, file = "data2019cleaned.csv")
