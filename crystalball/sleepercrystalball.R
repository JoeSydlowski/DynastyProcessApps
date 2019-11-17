library(jsonlite)
library(rvest)
library(tidyverse)
library(DT)
library(RColorBrewer)

leagueID<-467841245645893632
#leagueID<-425023252880957440

playoffweekstart<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID)))$settings$playoff_week_start-1

matchups<-tibble(week=c(1:playoffweekstart))

getmatchups<-function(i,LID){
  fromJSON(paste0("https://api.sleeper.app/v1/league/",LID,"/matchups/",i)) %>%
    #mutate(week=i) %>% 
    select(roster_id,points,matchup_id)
}

matchups<- matchups %>% 
  mutate(data=lapply(week,getmatchups,leagueID)) %>% 
  hoist(data,roster_id='roster_id',points='points',matchup_id='matchup_id') %>% 
  unnest(roster_id,points,matchup_id) %>%
  select(-data)


for (i in 1:playoffweekstart) {
  wm<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/matchups/",as.character(i))) %>%
    mutate(week=i) %>%
    select(roster_id,week,points,matchup_id)
  matchups<-bind_rows(matchups,wm)
  rm(wm,i)
}

teamlist<-fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters"))%>%
  select(owner_id,roster_id)

users<- fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/users"),flatten=TRUE) %>%
  select(owner=display_name,owner_id=user_id,teamname=metadata.team_name)%>%
  inner_join(teamlist,by="owner_id") %>% 
  mutate_all(as.character)

rm(teamlist)

matchups1<-matchups %>% 
  select(opp=roster_id,wk=week,opp_pts=points,m_id=matchup_id) %>%
  mutate(opp=as.character(opp),opp_pts=as.character(opp_pts)) %>%
  unite("merge",opp,opp_pts)

schedule<- matchups %>% 
  nest_join(matchups1,by=c('week'='wk','matchup_id'='m_id')) %>% 
  unnest_wider(matchups1) %>%
  unnest_longer(merge) %>%
  separate(merge,into=c('opp','opp_pts'),sep="_") %>%
  mutate(opp_pts=round(as.numeric(opp_pts),digits=2)) %>% 
  filter(roster_id!=opp) %>%
  mutate(wins=if_else(points>opp_pts,1,0)) %>%
  group_by(week)%>%
  mutate(allplaywins=rank(points)-1,gms=n()-1)%>%
  ungroup()

rm(matchups,matchups1)

standings<-schedule %>%
  filter(!is.na(points)) %>%
  group_by(roster_id)%>%
  summarize(wins=sum(wins),allplaywins=sum(allplaywins),weeks=n(),allplaygms=sum(gms)) %>%
  ungroup() %>% 
  mutate(losses=weeks-wins,allplaypct=round(allplaywins/allplaygms,digits=3),allplaylosses=allplaygms-allplaywins,roster_id=as.character(roster_id))

fullschedule<- schedule %>%
  filter(is.na(points)) %>% 
  select(roster_id,week,opp)%>%
  mutate(roster_id=as.character(roster_id)) %>% 
  nest_join(standings,by='roster_id',name='teaminfo') %>%
  nest_join(standings,by=c('opp'='roster_id'),name='oppinfo')%>%
  hoist(teaminfo,team_pct='allplaypct')%>%
  hoist(oppinfo,opp_pct='allplaypct')%>%
  mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3)) %>%
  select(roster_id,week,opp,team_pct,opp_pct,win_prob)

summary<-fullschedule %>% 
  group_by(roster_id) %>% 
  summarize(rosWins=sum(win_prob),rosGms=n()) %>%
  ungroup() %>% 
  left_join(standings,by='roster_id') %>% 
  left_join(users,by='roster_id') %>%
  mutate(rosLosses=rosGms-rosWins) %>% 
  select(Team=owner,`AllPlay%`=allplaypct,Wins=wins,Losses=losses,rosWins,rosLosses) %>% 
  mutate(TotalWins=Wins+rosWins,TotalLosses=Losses+rosLosses) %>% 
  arrange(desc(TotalWins))

fspivot<-fullschedule %>% 
  select(roster_id,week,win_prob) %>% 
  nest_join(users,by='roster_id') %>% 
  hoist(users,Team='owner') %>% 
  select(-roster_id,-users) %>% 
  pivot_wider(names_from = week, values_from=win_prob, names_prefix = "Week")
  



