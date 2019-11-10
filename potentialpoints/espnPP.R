library(jsonlite)
library(tidyverse)
library(DT)


#This is the one with the for loop. One day, Joe will teach me how to wrap reactives around this to make it shiny.

league_id<-251449

scoreweek<-4

optimal_lineups<-tibble()

espnbasic<- fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                       league_id,
                       '?view=mSettings',
                       '&view=mTeam' 
                       ),flatten=TRUE)

maxweek<-espnbasic$status$currentMatchupPeriod

name<-espnbasic$settings$name

teams<-espnbasic$teams %>% 
  select(id,primaryOwner, location, nickname) %>% 
  mutate(team_name=paste(location,nickname)) %>% 
  select(id,primaryOwner,team_name)

owners<-espnbasic$members %>% 
  select(id,owner_name=displayName) %>% 
  nest_join(teams,by=c('id'='primaryOwner'),name='teams') %>% 
  hoist(teams,team_id='id',team_name='team_name') %>% 
  select(-teams)

for (scoreweek in 1:4){

espn<- fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                       league_id,
                       '?scoringPeriodId=',
                       scoreweek,
                       '&view=mMatchupScore',
                       '&view=mBoxscore',
                       #'&view=mScoreboard',
                       #'&view=mTeam', 
                       #'&view=mRoster',
                       '&view=mSettings',
                       '&view=mRosterSettings'
                       #'&view=kona_player_info'
                       #'&view=mNav'
                       ),flatten=TRUE)

lineup_settings<-tibble(lineup_id=c(0,2,3,4,5,6,7,16,17,20,21,23,
                                   8,9,10,11,24,12,13,14,15),
                       pos=c('QB','RB','RB/WR','WR','WR/TE','TE',
                             'OP','DST','K','BE','IR','FLEX',
                             'DT','DE','LB','DL','EDR','CB','S','DB','DP'),
                       priority=c(1,2,5,3,6,4,8,9,10,0,0,11,
                                  12,13,16,14,15,17,18,19,20)) %>%
  arrange(lineup_id) %>% 
  mutate(lineup_id=as.character(lineup_id)) %>% 
  left_join(tibble(lineup_id=as.character(names(espn$settings$rosterSettings$lineupSlotCounts)),count=espn$settings$rosterSettings$lineupSlotCounts),
            by='lineup_id') %>% 
  filter(count!=0 & priority!=0) %>% 
  arrange(priority)
  

schedule<-espn$schedule %>%
  select(week=matchupPeriodId,away.teamId,away.entries=away.rosterForCurrentScoringPeriod.entries,
         home.teamId,home.entries=home.rosterForCurrentScoringPeriod.entries, 
         home.points=home.totalPoints,away.points=away.totalPoints) %>% 
  filter(away.entries!='NULL')

playerweeks <- schedule %>%
  select(
    week,
    home.teamId = away.teamId,
    home.entries = away.entries,
    home.points = away.points
  ) %>%
  bind_rows(schedule) %>%
  select(week,
         team_id = home.teamId,
         score = home.points,
         entries = home.entries) %>%
  hoist(
    entries,
    actual_lineup = 'lineupSlotId',
    player_id = 'playerId',
    points = 'playerPoolEntry.appliedStatTotal',
    player = 'playerPoolEntry.player.fullName',
    eligible = 'playerPoolEntry.player.eligibleSlots'
  ) %>%
  unnest(actual_lineup, player_id, points, player, eligible) %>%
  unnest_longer(eligible) %>% 
  mutate(eligible=as.character(eligible)) %>% 
  select(-entries)

unusedplayers<-playerweeks
starters<-tibble()

for (i in lineup_settings$priority) {
  pos<-lineup_settings %>% 
    filter(priority==i) %>% 
    nest_join(unusedplayers,by=c('lineup_id'='eligible')) %>% 
    unnest_wider(unusedplayers) %>% 
    unnest(-(1:3)) %>% 
    group_by(team_id) %>% 
    mutate(rank=rank(desc(points),ties.method=c('first'))) %>% 
    filter(rank<=count)
  
  starters<-bind_rows(starters,pos) %>% 
    arrange(team_id,priority,desc(points))
  
  unusedplayers<-unusedplayers %>% 
    anti_join(pos,by=c('player_id'))
}

optimal_lineups<-bind_rows(optimal_lineups,starters)

optimal_lineups

}

summary_week<-optimal_lineups %>% 
  group_by(team_id,week) %>% 
  summarise(actual_score=mean(score),optimal_score=sum(points)) %>% 
  ungroup() %>% 
  left_join(owners,by='team_id') %>% 
  arrange(week,desc(optimal_score)) %>% 
  select(Team=team_name,Week=week,`Actual Score`=actual_score,`Optimal Score`=optimal_score)

unused_summary<-unusedplayers %>% 
  select(-eligible) %>% 
  distinct()
