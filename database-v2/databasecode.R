#library(httr)
library(jsonlite)
library(tidyverse)

options(stringsAsFactors = FALSE)



## Replicating the "MFL" query of my database builder

mfl<-fromJSON("https://www03.myfantasyleague.com/2019/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1",flatten=TRUE)%>%
    data.frame()

names(mfl)<-gsub("players.player.","",names(mfl))

mfl<-select(mfl,c("id","name","position","team",
                  "draft_year","stats_global_id","fleaflicker_id",
                  "rotoworld_id","nfl_id","espn_id","sportsdata_id",
                  "draft_round","draft_pick","height",
                  "weight","college","birthdate")) %>%
        filter(position %in% c("QB","RB","WR","TE"))%>%
        mutate(birthdate = as.Date(as.POSIXct(as.numeric(birthdate),origin="1970-01-01")),
               age = round((Sys.Date()-birthdate)/365.25,digits=1))

# Replicating the "Sleeper" query of my database builder

sleeper<-fromJSON("https://api.sleeper.app/v1/players/nfl")

sleeper.df<-data.frame(do.call("rbind",sleeper)) %>%
    select(c("first_name","last_name", "team","position",
           "fantasy_data_id","rotowire_id","rotoworld_id",
           "sportradar_id","espn_id","gsis_id",
           "stats_id","yahoo_id"))

sleeper.df<-mutate(sleeper.df,sleeper_id=rownames(sleeper.df)) %>%
  filter(position %in% c("QB", "RB", "FB", "WR", "TE"))

sleeper.df[sleeper.df=="NULL"]<-NA
rm(sleeper)

# Get Spotrac

