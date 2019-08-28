library(shiny)
library(tidyverse)
library(jsonlite)
library(rvest)
library(DT)
library(lubridate)
library(RColorBrewer)

# Local Files
database<-read.csv("database.csv") %>% select(sleeper_id,mergename,pos,team,age,dynoECR,dynpECR,rdpECR,salary_avg,draft_year,fa_year)
values<-read.csv("values.csv")

# For Eventual Shiny Inputs

username<-'solarpool' #eventual shiny inputbox, onclick pushes to create the leaguelist

user_id<-fromJSON(paste0("https://api.sleeper.app/v1/user/",username),flatten=TRUE)$user_id
leaguelist<-fromJSON(paste0("https://api.sleeper.app/v1/user/",user_id,"/leagues/nfl/",year(Sys.Date())))%>%
  select(name,league_id)

leagueID<-leaguelist$league_id[3] #shiny input -> select leagueID, then carry on

leaguename<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID)),flatten=TRUE)$name

#User-related functions

userlist<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters"))%>%
  select(owner_id,roster_id)

users<- fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/users"),flatten=TRUE) %>%
  select(owner=display_name,owner_id=user_id,teamname=metadata.team_name)%>%
  merge(userlist)

rm(userlist)

basepicks<-

tradedpicks<- fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/traded_picks"),flatten=TRUE)%>%
  filter(season!=2019)%>%
  arrange(owner_id,season,round,roster_id)%>%
  merge(users,)

rosters<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters")) %>% 
  select(roster_id,players,owner_id) %>% 
  unnest() %>%
  merge(users,by.x="owner_id",by.y="owner_id") %>% 
  merge(database,by.x="players",by.y="sleeper_id",all.x=TRUE,suffixes=c("",".db")) %>% 
  merge(values,all.x=TRUE)%>%
  select(roster_id,owner,teamname,sleeper_id=players,mergename,pos,team,age,dynoECR,dynpECR,rdpECR,value=X1QBValue,salary_avg,fa_year) %>%
  filter(!is.na(.$pos))


# REPORTS - POSITIONAL DYNASTY VALUE SUMMARY

pospivot<-group_by(rosters,owner,pos) %>%
  summarize(value = sum(value,na.rm=TRUE)) %>% ungroup() %>%
  group_by(owner) %>% mutate(total=sum(value)) %>%  ungroup() %>%
  spread(pos,value)%>%
  gather(pos,value,total:WR)%>%
  group_by(pos) %>% mutate(total = sum(value)) %>% ungroup()%>%
  transmute(owner = owner, pos = pos, pct = round(value/total,digits=4))  %>%
  spread(pos,pct) %>%
  select(owner,qb=QB,rb=RB,wr=WR,te=TE,total)%>%
  arrange(desc(total))

colors<-colorRampPalette(brewer.pal(3,'PuOr'))

breaks<-function(colnum){
  breakvalue<-quantile(range(pospivot[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
  return(breakvalue)
  }

datatable(pospivot,
          rownames=FALSE,
          colnames=c("Owner","QB","RB","WR","TE","Total"),
          options(
            paging=FALSE,
            searching=FALSE
          )) %>%
  formatPercentage(c(2:6),2)%>%
  formatStyle(2, backgroundColor = styleInterval(breaks(2),colors(length(breaks(2))+1)))%>%
  formatStyle(3, backgroundColor = styleInterval(breaks(3),colors(length(breaks(3))+1)))%>%
  formatStyle(4, backgroundColor = styleInterval(breaks(4),colors(length(breaks(4))+1)))%>%
  formatStyle(5, backgroundColor = styleInterval(breaks(5),colors(length(breaks(5))+1)))%>%
  formatStyle(6, backgroundColor = styleInterval(breaks(6),colors(length(breaks(6))+1)))

  
# REPORTS - FREE AGENTS

freeagents<-database %>%
  mutate(sleeper_id=as.character(sleeper_id))%>%
  anti_join(rosters,by=c("sleeper_id"="sleeper_id"))