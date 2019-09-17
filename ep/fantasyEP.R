library(tidyverse)
library(dplyr)
library(nflscrapR)
library(ggplot2)
library(lubridate)


# Goals: Recreate Kevin Cole's EP for passing, rushing, and receiving and present as a web-app.

# Data Preprocessing

# 2014-2018 (five years worth of pbp) cleaned to dropbacks and not-dropbacks (as per Ben Baldwin's tutorial)

setwd("~/GitHub/DynastyProcess-Apps/ep")

filelist<-data_frame(l=list.files()) %>%
  filter(grepl('reg_pbp',l))

pbp<-data_frame()

for (f in filelist$l){
  df<-read.csv(f)
  pbp<-bind_rows(pbp,df)
  rm(df,f)
  }

# below labels no-play events as 'pass' and 'rush' - i.e. taking into account penalties and relabelling scrambles/sacks as pass attempts

pbp_baldwin <- pbp %>% 
  filter(!is.na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0)
  )%>%
  filter(pass==1 | rush==1)

# runEP - rushes, yards gained, yfog, TDs, fumbles

pbp_runs <-pbp_baldwin %>%
  filter(rush==1, play_type!="no_play") %>% 
  mutate(
    yardsfromgoal=case_when(home_team==posteam ~ as.integer(100-yardline_100),
                            away_team==posteam ~ yardline_100)
  ) %>%
  select(play_id,game_id,game_date,posteam,defteam,yardsfromgoal,
         yrdln,ydstogo,desc,rusher_player_id,rusher_player_name,yards_gained,rush_touchdown,fumble)
  
ggplot(pbp_runs, aes(x = ydstogo, y = yards_gained)) + 
  stat_smooth()+ xlim(1,20) + ylim(0,8)

pbp_runs%>%group_by(yardsfromgoal)%>%mutate(tdrate=sum(rush_touchdown)/n())%>% ggplot(aes(x=yardsfromgoal,y=rush_touchdown))+geom_smooth()



