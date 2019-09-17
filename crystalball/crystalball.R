library(jsonlite)
library(rvest)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(lubridate)

id<-54040

franchises<-fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=league&L=",id,"&APIKEY=&JSON=1"))$league$franchises$franchise %>%
  select(ownerid=id,owner=name)

standings<-fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=leagueStandings&L=",id,"&APIKEY=&JSON=1"))$leagueStandings$franchise %>% 
  select(ownerid=id,pointsfor=pf,potentialpoints=pp,starts_with('h2h'),starts_with('all_play')) %>%
  mutate_at(vars(starts_with('all_play')),as.numeric) %>%
  mutate(record=paste0(h2hw,"-",h2ht,"-",h2hl),
         allplaypct=all_play_w/(all_play_w+all_play_t+all_play_l)) %>%
  select(-starts_with('all_play')) %>%
  inner_join(franchises)
  
schedule <-fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=schedule&L=",id,"&APIKEY=&JSON=1"))$schedule$weeklySchedule %>% 
  unnest(matchup) %>% 
  unnest_wider(franchise) %>%
  filter(result=='NA') %>% 
  select(id,week) %>%
  hoist(id,team=1,opp=2)

fullschedule <- schedule %>%
  select(week,team=opp,opp=team) %>%
  bind_rows(schedule) %>%
  mutate(week=as.numeric(week)) %>% 
  arrange(week,team) %>%
  nest_join(standings, by=c("team"="ownerid"),name="teaminfo") %>%
  nest_join(standings,by=c("opp"="ownerid"),name="oppinfo") %>%
  hoist(teaminfo,team_name="owner",team_pct="allplaypct") %>%
  hoist(oppinfo,opp_name="owner",opp_pct="allplaypct") %>%
  select(-teaminfo,-oppinfo,-opp) %>%
  mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3))

expectedwins<-fullschedule %>%
  group_by(team,team_name) %>%
  summarize(ewins=sum(win_prob,na.rm=TRUE),
            elosses=n()-ewins) %>%
  ungroup() %>%
  nest_join(standings,by=c('team'='ownerid')) %>%
  hoist(standings,h2hw="h2hw",h2ht="h2ht",h2hl="h2hl") %>%
  mutate_at(vars("h2hw","h2ht","h2hl"),as.numeric) %>%
  mutate(total_wins=ewins+h2hw, total_losses=elosses+h2hl) %>%
  select(team,team_name,wins=h2hw,losses=h2hl,ewins,elosses,total_wins,total_losses) %>%
  arrange(desc(total_wins))

