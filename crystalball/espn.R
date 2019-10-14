library(jsonlite)
library(tidyverse)

espn<- fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                       '1178049',
                       '?view=mMatchupScore',
                       # '&view=mBoxscore',
                       # '&view=mScoreboard',
                       '&view=mTeam', 
                       # '&view=mRoster',
                       '&view=mNav'),flatten=TRUE)

owners<-espn$members %>% 
  select(id,displayName) %>% 
  nest_join(espn$teams,by=c('id'='primaryOwner'),name='teams') %>% 
  hoist(teams,teamid='id') %>% 
  select(-teams)

teams<-espn$teams %>% 
  select(id,primaryOwner)

leaguename<-espn$settings$name

schedule1<-espn$schedule %>% 
  select(winner, contains('id'),ends_with('totalPoints')) %>% 
  rename(gameid=id,week=matchupPeriodId,
         team_id=away.teamId, opp_id=home.teamId,
         team_points=away.totalPoints, opp_points=home.totalPoints)

schedule<-espn$schedule %>% 
  select(winner, contains('id'),ends_with('totalPoints')) %>% 
  rename(gameid=id,week=matchupPeriodId,
         team_id=home.teamId, opp_id=away.teamId,
         team_points=home.totalPoints, opp_points=away.totalPoints) %>% 
  bind_rows(schedule1) %>% 
  arrange(week,team_id)
  
standings<-schedule %>% 
  filter(winner!='UNDECIDED') %>% 
  group_by(week) %>% 
  mutate(wins=if_else(team_points>opp_points,1,0),
         APgms=n()-1,
         APwins=rank(team_points)-1) %>% 
  ungroup() %>% 
  group_by(team_id) %>% 
  summarize(wins=sum(wins),allplaywins=sum(APwins),weeks=n(),allplaygms=sum(APgms)) %>% 
  mutate(losses=weeks-wins,
         allplaypct=round(allplaywins/allplaygms,digits=3),
         allplaylosses=allplaygms-allplaywins)

fullschedule<- schedule %>%
  filter(winner=='UNDECIDED') %>% 
  select(team_id,week,opp_id)%>%
  nest_join(standings,by='team_id',name='teaminfo') %>%
  nest_join(standings,by=c('opp_id'='team_id'),name='oppinfo')%>%
  hoist(teaminfo,team_pct='allplaypct')%>%
  hoist(oppinfo,opp_pct='allplaypct')%>%
  mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3)) %>%
  select(team_id,week,opp_id,team_pct,opp_pct,win_prob)

summary<-fullschedule %>% 
  group_by(team_id) %>% 
  summarize(rosWins=sum(win_prob),rosGms=n()) %>%
  ungroup() %>% 
  left_join(standings,by='team_id') %>%
  mutate(team_id=as.character(team_id)) %>% 
  left_join(owners,by=c('team_id'='rosterid')) %>%
  mutate(rosLosses=rosGms-rosWins) %>% 
  select(Team=displayName,`AllPlay%`=allplaypct,Wins=wins,Losses=losses,rosWins,rosLosses) %>% 
  mutate(TotalWins=Wins+rosWins,TotalLosses=Losses+rosLosses) %>% 
  arrange(desc(TotalWins))

fspivot<-fullschedule %>% 
  select(team_id,week,win_prob) %>%
  mutate(team_id=as.character(team_id)) %>% 
  nest_join(owners,by=c('team_id'='rosterid') %>% 
  hoist(owners,Team='displayName') %>% 
  select(-team_id,-owners) %>% 
  pivot_wider(names_from = week, values_from=win_prob, names_prefix = "Week")  

