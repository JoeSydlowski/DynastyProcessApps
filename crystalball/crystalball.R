library(jsonlite)
library(rvest)
library(httr)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(lubridate)

id<-54040

username<-'solarpool'
password<-'M^#63tho'


# MFL code

# for authentication somewhere in the future, needs rebuilding m_franchises/m_standings/m_schedule to accomplish
m_cookie<-GET(paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",username,"&PASSWORD=",URLencode(password,reserved=TRUE),"&XML=1"))$cookies$value %>%
    URLencode(reserved=TRUE)

m_leagues<-GET("https://www61.myfantasyleague.com/2019/export?TYPE=myleagues&FRANCHISE_NAMES=1&JSON=1",
               set_cookies("MFL_USER_ID"=m_cookie()[1],"MFL_PW_SEQ"=m_cookie()[2]),accept_json()) %>%
  content("text") %>% fromJSON() %>% .$leagues %>% .$league %>% 
  mutate(LeagueID=str_sub(url,start=-5)) %>% 
  select(League=name,Team=franchise_name,LeagueID)




m_franchises<-GET(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=league&L=",id,"&APIKEY=&JSON=1"),add_headers(paste0("Cookie: MFL_USER_ID=",authcookie))) %>% 
  content(type = 'application/json')

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
  mutate(
    team_pct = round(team_pct,digits=3),
    opp_pct = round(opp_pct, digits=3),
    win_prob=round(team_pct/(team_pct+opp_pct),digits=3))

expectedwins<-fullschedule %>%
  group_by(team,team_name) %>%
  summarize(ewins=sum(win_prob,na.rm=TRUE),
            elosses=n()-ewins) %>%
  ungroup() %>%
  nest_join(standings,by=c('team'='ownerid')) %>%
  hoist(standings,h2hw="h2hw",h2ht="h2ht",h2hl="h2hl") %>%
  mutate_at(vars("h2hw","h2ht","h2hl"),as.numeric) %>%
  mutate(TotalWins=ewins+h2hw, TotalLosses=elosses+h2hl) %>%
  select(Team = team_name,Wins=h2hw,Losses=h2hl,eWins=ewins,eLosses=elosses,TotalWins,TotalLosses) %>%
  arrange(desc(TotalWins))

fspivot<-fullschedule %>%
  mutate(weekname=paste("Week",week)) %>%
  select(weekname,team_name,win_prob) %>%
  pivot_wider(names_from=weekname,values_from = win_prob) %>%
  rename(Team=team_name)

brks<-function(tib){
  breakvalue<-quantile(range(fspivot),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
  return(breakvalue)
}

colors<-colorRampPalette(brewer.pal(3,'PuOr'))

fs_tbl<-renderDT(fspivot)%>%
    formatStyle(2:12,backgroundColor = styleInterval(quantile(range(list(0,1)),probs=seq(0.05,0.95,0.05),na.rm=TRUE),colors(20)))


  
