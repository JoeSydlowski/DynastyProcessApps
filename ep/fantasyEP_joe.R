library(tidyverse)
library(dplyr)
library(nflscrapR)
library(ggplot2)
library(lubridate)


# Goals: Recreate Kevin Cole's EP for passing, rushing, and receiving and present as a web-app.

# Data Preprocessing

# 2014-2018 (five years worth of pbp) cleaned to dropbacks and not-dropbacks (as per Ben Baldwin's tutorial)

#setwd("~/GitHub/DynastyProcess-Apps/ep")
setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')

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
  select(play_id,game_id,game_date,posteam,defteam,yardsfromgoal,
         yrdln,ydstogo,desc,rusher_player_id,rusher_player_name,yards_gained,rush_touchdown,fumble, yardline_100)

ggplot(pbp_runs, aes(x = ydstogo, y = yards_gained)) + 
  geom_smooth()+ xlim(1,20) + ylim(0,8)

pbp_runs2 <-pbp_baldwin %>%
  filter(rush==1, play_type!="no_play") %>% 
  group_by(ydstogo) %>%
  summarise(median_gain = median(yards_gained),
            avg_gain = mean(yards_gained),
            plays = n())

ggplot(pbp_runs2, aes(x = ydstogo, y = plays)) +
  geom_point()  + 
  xlim(1,20)

ggplot(pbp_runs2, aes(x = ydstogo, y = median_gain)) +
  geom_point()  + 
  geom_smooth() +
  xlim(1,20) +
  ylim(0,8)

df <- pbp_runs %>%
  #select(yardsfromgoal,rush_touchdown) %>%
  dplyr::group_by(yardline_100) %>% 
  summarise(tdrate=mean(rush_touchdown),
            count = n(),
            tds = sum(rush_touchdown==1),
            fmrate = mean(fumble),
            avg_gain = mean(yards_gained))

ggplot(df, aes(x=yardline_100,y=avg_gain)) + 
  geom_point() +
  geom_smooth()

ggplot(df, aes(x=yardline_100,y=tdrate)) + 
  geom_point()

ggplot(df, aes(x=yardline_100,y=count)) + 
  geom_point() +
  geom_smooth()

