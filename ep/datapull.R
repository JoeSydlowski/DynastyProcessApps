library(dplyr)
library(nflscrapR)

setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')
#setwd("/srv/shiny-server/DynastyProcess/ep")

ids <- scrape_game_ids(2019, type = "reg", weeks = c(1:5)) #%>%
#filter(state_of_game == "POST")

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