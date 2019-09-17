#library(httr)
library(jsonlite)
library(tidyverse)
library(rvest)

options(stringsAsFactors = FALSE)

# Hardcoded data files
teamIDs<-read.csv("teamIDs.csv") #teamdb because am lazy to fix nomenclature
stats2018<-read.csv("stats2018.csv") #stat-set built from NFLScrapR/PFR -> automation a project for 2019 offseason
rookieadp<-read.csv("rookie-adp.csv") #built from mizelle (really, DLF) ADP for rookie pick value analysis

## Replicating the "MFL" query of database builder

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
spotrac<-read_html("https://www.spotrac.com/nfl/contracts") %>%
  html_node(xpath='/html/body/div[2]/div[2]/div/div/div[1]/div/div[3]/table') %>%
  html_table()%>%
  separate(Player,c("name",NA),sep=' \\(') %>%
  separate(Team,c("team",NA),sep='Signed')

spotrac$name<-gsub(" Jr.","",spotrac$name)
spotrac$name<-gsub(" Sr.","",spotrac$name)
spotrac$name<-gsub(" IV","",spotrac$name)
spotrac$name<-gsub(" III","",spotrac$name)
spotrac$name<-gsub(" II","",spotrac$name)
spotrac$name<-gsub("\\'","",spotrac$name)
spotrac$name<-gsub("\\.","",spotrac$name)
spotrac$name<-gsub("St Brown","StBrown",spotrac$name)
spotrac$pos<-gsub("FB","RB",spotrac$pos)

spotrac<-separate(spotrac,name,c("firstname","lastname"),sep=" ", extra="merge") %>% 
  transform(firstname=mapply(sub,lastname,"",firstname,fixed=TRUE)) %>%
  unite("name",firstname,lastname,sep=" ")%>%
  merge(teamids,by.x="team",by.y="spotrac",suffixes=c("",".t")) %>%
  select(name,team=mfl,pos=Pos,avg_salary=Average.Salary,fa_yr=Free.Agent)

# Get FantasyPros

