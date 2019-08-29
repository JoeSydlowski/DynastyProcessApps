library(shiny)
library(tidyverse)
library(jsonlite)
library(rvest)
library(DT)
library(lubridate)
library(RColorBrewer)

# Local Files
database<-read.csv("database.csv") %>% select(sleeper_id,mergename,pos,team,age,dynoECR,dynpECR,rdpECR,
                                              salary_avg,draft_year,fa_year)
values<-read.csv("values.csv",fileEncoding="UTF-8-BOM")
projections<-read.csv("projections.csv",fileEncoding="UTF-8-BOM")

# For Eventual Shiny Inputs

username<-'solarpool' #eventual shiny inputbox, onclick pushes to create the leaguelist

user_id<-fromJSON(paste0("https://api.sleeper.app/v1/user/",username),flatten=TRUE)$user_id
leaguelist<-fromJSON(paste0("https://api.sleeper.app/v1/user/",user_id,"/leagues/nfl/",year(Sys.Date())))%>%
  select(name,league_id)

leagueID<-leaguelist$league_id[3] #shiny input -> select leagueID, then carry on

leaguename<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID)),flatten=TRUE)$name

#User-related functions

teamlist<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters"))%>%
  select(owner_id,roster_id)

users<- fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/users"),flatten=TRUE) %>%
  select(owner=display_name,owner_id=user_id,teamname=metadata.team_name)%>%
  inner_join(teamlist,by="owner_id")


tradedpicks<- fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/traded_picks"),
                       flatten=TRUE)%>%
  filter(season!=2019)

futurepicks <- tibble(
  roster_id = c(1:length(teamlist$roster_id)),
  owner_id = c(1:length(teamlist$roster_id)),
  season = list(seq(2020, 2022, 1)),
  round = list(seq(1, 4, 1))
  ) %>% 
  unnest(season,.drop=FALSE) %>%
  unnest(round)%>%
  mutate(season=as.character(season))%>%
  anti_join(tradedpicks,by=c("roster_id"="roster_id","season"="season","round"='round'))%>%
  bind_rows(tradedpicks)%>%
  arrange(owner_id,season,round,roster_id)%>%
  inner_join(users,by=c("owner_id"="roster_id"),suffix=c("","_owner"))%>%
  inner_join(users,by=c("roster_id"="roster_id"),suffix=c("","_original"))%>%
  select(owner_id,owner,season,round,roster_id,owner_original)%>%
  mutate(roundname= case_when(
                              round==1~'1st',
                              round==2~'2nd',
                              round==3~'3rd',
                              round==4~'4th'),
         mergename=paste(season,roundname)) %>%
  select(roster_id=owner_id,owner,season,round,original_id=roster_id,owner_original,mergename)%>%
  inner_join(values,by=c("mergename"="mergename"),keep=TRUE)%>%
  mutate_all(as.character)




  
rosters<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters")) %>% 
  select(roster_id,players,owner_id) %>% 
  unnest() %>%
  merge(users,by.x="owner_id",by.y="owner_id") %>% 
  merge(database,by.x="players",by.y="sleeper_id",all.x=TRUE,suffixes=c("",".db")) %>% 
  merge(values,all.x=TRUE)%>%
  mutate_all(as.character)%>%
  bind_rows(futurepicks)%>%
  select(owner_id,owner,teamname,sleeper_id=players,mergename,pos,team,age,dynoECR,dynpECR,rdpECR,value=X1QBValue,salary_avg,fa_year) %>%
  filter(!is.na(.$pos))%>%
  mutate_at(vars(age,dynoECR,dynpECR,rdpECR,value,salary_avg),as.numeric) %>%
  left_join(projections,c('pos'='pos','rdpECR'='rank'))
  

# REPORTS - POSITIONAL DYNASTY VALUE SUMMARY

pivot_dpos<-group_by(rosters,owner,pos) %>%
  summarize(value = sum(value,na.rm=TRUE)) %>% ungroup() %>%
  group_by(owner) %>% mutate(total=sum(value,na.rm=TRUE)) %>%  ungroup() %>%
  spread(pos,value)%>%
  gather(pos,value,total:WR)%>%
  group_by(pos) %>% mutate(total = sum(value,na.rm=TRUE)) %>% ungroup()%>%
  transmute(owner = owner, pos = pos, pct = round(value/total,digits=4))  %>%
  spread(pos,pct) %>%
  select(owner,qb=QB,rb=RB,wr=WR,te=TE,PICK,total)%>%
  arrange(desc(total))%>%
  mutate_all(~replace(.,is.na(.),0))

colors<-colorRampPalette(brewer.pal(3,'PuOr'))

brks_dpos<-function(colnum){
  breakvalue<-quantile(range(pivot_dpos[colnum]),probs=seq(0.01,0.99,0.01),na.rm=TRUE)
  return(breakvalue)
  }

datatable(pivot_dpos,
          rownames=FALSE,
          colnames=c("Owner","QB","RB","WR","TE","PICK","Total"),
          options(
            paging=FALSE,
            searching=FALSE
          )) %>%
  formatPercentage(c(2:7),2)%>%
  formatStyle(2, backgroundColor = styleInterval(brks_dpos(2),colors(length(brks_dpos(2))+1)))%>%
  formatStyle(3, backgroundColor = styleInterval(brks_dpos(3),colors(length(brks_dpos(3))+1)))%>%
  formatStyle(4, backgroundColor = styleInterval(brks_dpos(4),colors(length(brks_dpos(4))+1)))%>%
  formatStyle(5, backgroundColor = styleInterval(brks_dpos(5),colors(length(brks_dpos(5))+1)))%>%
  formatStyle(6, backgroundColor = styleInterval(brks_dpos(6),colors(length(brks_dpos(6))+1)))%>%
  formatStyle(7, backgroundColor = styleInterval(brks_dpos(7),colors(length(brks_dpos(7))+1)))
  
# REPORTS - POSITIONAL REDRAFT VALUE SUMMARY

starters <- select(rosters, owner, mergename, pos, team, age, pts) %>%
  filter(pos != "PICK") %>%
  arrange(owner, pos, desc(pts)) %>%
  group_by(owner, pos) %>%  mutate(posrank = rank(desc(pts), ties.method = 'first')) %>%  ungroup() %>%
  mutate(
    starter = case_when(
      pos == "QB" & posrank == 1 ~ "QB",
      pos == "RB" & posrank <= 2 ~ "RB",
      pos == "WR" & posrank <= 2 ~ "WR",
      pos == "TE" & posrank == 1 ~ "TE"
    ),
    flex_elig = case_when(pos != "QB" & is.na(starter) ~ 1)) %>%
  group_by(owner, flex_elig) %>%
  mutate(
    flexrank = case_when(flex_elig==1 ~ rank(desc(pts), ties.method = 'first')),
    flex = case_when(flexrank <= 2 ~ "FLEX"),
    lineup = case_when(!is.na(starter) ~ paste0(starter,posrank),
                       flex == "FLEX" ~ paste0(flex,flexrank)))%>%
  ungroup()


pivot_rpos <- starters %>%
  mutate_at(vars(lineup),  ~ replace(., is.na(.), "Bench")) %>%
  group_by(owner, lineup) %>%
  summarize(pts = sum(pts, na.rm = TRUE)) %>% ungroup() %>%
  mutate(bench = case_when(lineup == "Bench" ~ 1)) %>%
  group_by(owner, bench) %>% mutate(total = sum(pts, na.rm = TRUE)) %>%  ungroup() %>%
  spread(lineup, pts) %>%
  gather(lineup, pts, 3:ncol(.)) %>%
  drop_na(pts) %>%
  mutate(
    lineup = case_when(
      bench == 1 & lineup == "total" ~ "NA",
      lineup == "total" ~ "Starters Total",
      bench == 1 ~ "Bench",
      TRUE ~ lineup)
    )%>%
  transmute(owner = owner, lineup = lineup, pts=pts)  %>%
  spread(lineup,pts) %>%
  select(owner, starts_with("QB"), starts_with("RB"), starts_with("WR"), starts_with("TE"), starts_with("FLEX"), starters=`Starters Total`, bench=Bench)%>%
  mutate(team=starters+bench)%>%
  arrange(desc(starters))%>%
  mutate_all(~replace(.,is.na(.),0))


brks_rpos<-function(colnum){
  breakvalue<-quantile(range(pivot_rpos[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
  return(breakvalue)
}

datatable(pivot_rpos,
          rownames=FALSE,
          options(
            paging=FALSE,
            searching=FALSE
          )) %>%
  formatStyle(2, backgroundColor = styleInterval(brks_rpos(2),colors(length(brks_rpos(2))+1)))%>%
  formatStyle(3, backgroundColor = styleInterval(brks_rpos(3),colors(length(brks_rpos(3))+1)))%>%
  formatStyle(4, backgroundColor = styleInterval(brks_rpos(4),colors(length(brks_rpos(4))+1)))%>%
  formatStyle(5, backgroundColor = styleInterval(brks_rpos(5),colors(length(brks_rpos(5))+1)))%>%
  formatStyle(6, backgroundColor = styleInterval(brks_rpos(6),colors(length(brks_rpos(6))+1)))%>%
  formatStyle(7, backgroundColor = styleInterval(brks_rpos(7),colors(length(brks_rpos(7))+1)))%>%
  formatStyle(8, backgroundColor = styleInterval(brks_rpos(8),colors(length(brks_rpos(8))+1)))%>%
  formatStyle(9, backgroundColor = styleInterval(brks_rpos(9),colors(length(brks_rpos(9))+1)))%>%
  formatStyle(10, backgroundColor = styleInterval(brks_rpos(10),colors(length(brks_rpos(10))+1)))%>%
  formatStyle(11, backgroundColor = styleInterval(brks_rpos(11),colors(length(brks_rpos(11))+1)))%>%
  formatStyle(12, backgroundColor = styleInterval(brks_rpos(12),colors(length(brks_rpos(12))+1)))
  
# REPORTS - FREE AGENTS

freeagents<-database %>%
  mutate(sleeper_id=as.character(sleeper_id))%>%
  anti_join(rosters,by=c("sleeper_id"="sleeper_id"))