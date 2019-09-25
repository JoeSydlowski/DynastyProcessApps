library(jsonlite)
library(tidyverse)

espn<- fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                       '1178049',
                       '?scoringPeriodId=2',
                       '&view=mMatchupScore',
                       '&view=mBoxscore',
                       '&view=mScoreboard',
                       '&view=mTeam', 
                       '&view=mRoster',
                       '&view=mSettings',
                       '&view=mRosterSettings',
                       '&view=kona_player_info',
                       '&view=mNav'),flatten=TRUE)

teams<-espn$teams %>% 
  select(id,primaryOwner)

owners<-espn$members %>% 
  select(id,displayName) %>% 
  nest_join(espn$teams,by=c('id'='primaryOwner'),name='teams') %>% 
  hoist(teams,teamid='id') %>% 
  select(-teams)

leaguename<-espn$settings$name

startinglineup<-tibble(lineup_id=c(0,2,3,4,5,6,7,16,17,20,21,23,8,9,10,24,12,13,14,15),
                       pos=c('QB','RB','RB/WR','WR','WR/TE','TE','OP','DST','K','BE','IR','FLEX','DT','DE','LB','EDR','CB','S','DB','DP')) %>%
  arrange(lineup_id) %>% 
  mutate(lineup_id=as.character(lineup_id)) %>% 
  left_join(tibble(lineup_id=as.character(names(espn$settings$rosterSettings$lineupSlotCounts)),count=espn$settings$rosterSettings$lineupSlotCounts),
            by='lineup_id')
  

schedule<-espn$schedule %>%
  select(week=matchupPeriodId,away.teamId,away.entries=away.rosterForCurrentScoringPeriod.entries,
         home.teamId,home.entries=home.rosterForCurrentScoringPeriod.entries, 
         home.points=home.totalPoints,away.points=away.totalPoints) %>% 
  filter(away.entries!='NULL')

df <- schedule %>%
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
    lineup_id = 'lineupSlotId',
    player_id = 'playerId',
    points = 'playerPoolEntry.appliedStatTotal',
    player = 'playerPoolEntry.player.fullName',
    eligible = 'playerPoolEntry.player.eligibleSlots'
  ) %>%
  unnest(lineup_id, player_id, points, player, eligible) %>%
  mutate(lineup_id = as.character(lineup_id)) %>%
  select(-entries) %>% 
  unnest(eligible) %>% 
  mutate(eligible=as.character(eligible)) %>% 
  left_join(startinglineup,by=c('eligible'='lineup_id')) %>% 
  select(-eligible) %>% 
  pivot_wider(names_from=pos,values_from(count),values_fn=sum)


