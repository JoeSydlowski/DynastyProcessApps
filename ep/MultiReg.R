library(tidyverse)
library(dplyr)
library(nflscrapR)
library(ggplot2)
library(lubridate)
library(stargazer)
library(MASS)

ids <- scrape_game_ids(2019, type = "reg", weeks = 1)
id <- ids %>% pull(game_id)

df2019 <-  data.frame()
pbp_data <- for (i in id)
{df <- scrape_json_play_by_play(i)
 df2019 <- rbind(df2019,df)}

rushdf2019 <- df2019 %>% 
  filter(!is.na(epa),
         play_type %in% c("run")) %>%
  mutate(pass_binary = if_else(play_type == "run", 0, 1),
         TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         rushFP = 6*touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
         logremaining = log(remaining),
         run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap)))


# Goals: Create new method to predict FP for passing, rushing, and receiving and present as a web-app.

# Data Preprocessing

# 2014-2018 (five years worth of pbp) cleaned to dropbacks and not-dropbacks (as per Ben Baldwin's tutorial)

#setwd("~/GitHub/DynastyProcess-Apps/ep")
setwd('C:/Users/syd23/OneDrive/Documents/DynastyProcess/ep')

set.seed(100)  # setting seed to reproduce results of random sampling

filelist<-data_frame(l=list.files()) %>%
  filter(grepl('reg_pbp',l))

pbp<-data_frame()

for (f in filelist$l){
  df<-read.csv(f)
  pbp<-bind_rows(pbp,df)
  rm(df,f)
}

# below labels no-play events as 'pass' and 'rush' - i.e. taking into account penalties and relabelling scrambles/sacks as pass attempts

rushdf <- pbp %>% 
  filter(!is.na(epa),
         play_type %in% c("run")) %>%
  mutate(pass_binary = if_else(play_type == "run", 0, 1),
         TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         rushFP = 6*touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
         logremaining = log(remaining),
         run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap)))

rushtrainingRowIndex <- sample(1:nrow(rushdf), 0.8*nrow(rushdf))  # row indices for training data
rushtrainingData <- rushdf[rushtrainingRowIndex, ]  # model training data
rushtestData  <- rushdf[-rushtrainingRowIndex, ]   # test data

recdf <- pbp %>% 
  filter(!is.na(epa),
         play_type %in% c("pass")) %>%
  mutate(pass_binary = if_else(play_type == "run", 0, 1),
         TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         recFP = 6*touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         remaining = if_else(game_seconds_remaining == 0, 1, game_seconds_remaining),
         logremaining = log(remaining)
         )


rectrainingRowIndex <- sample(1:nrow(recdf), 0.8*nrow(recdf))  # row indices for training data
rectrainingData <- recdf[rectrainingRowIndex, ]  # model training data
rectestData  <- recdf[-rectrainingRowIndex, ]   # test data

lmMod <- lm(recFP ~ logyardline + yardline_100  + factor(down) 
            + air_yards + shotgun  + pass_location, data=rectrainingData)  # build the model
stepAIC(lmMod)
stargazer(lmMod, type = "text")

lmMod2 <- lm(rushFP ~ logyardline+ yardlinesq + yardline_100 + factor(down)
             + shotgun + run_gap2 , data=rushtrainingData)  # build the model
stepAIC(lmMod2)
stargazer(lmMod2, type = "text")

rectrainingData$recEP <- predict(lmMod, rectrainingData)
rushtrainingData$rushEP <- predict(lmMod2, rushtrainingData)


explore <- rectrainingData %>%
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, recFP, recEP) %>%
  group_by(yardline_100) %>% 
  summarise(
            avg_EP = mean(recEP, na.rm = TRUE),
            avg_FP = mean(recFP, na.rm = TRUE))

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_EP), color = 'red') + 
  geom_point(aes(x = yardline_100, y = avg_FP), color = 'green')

exploreay <- rectrainingData %>%
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, recFP, recEP) %>%
  group_by(air_yards) %>% 
  summarise(
    avg_EP = mean(recEP, na.rm = TRUE),
    avg_FP = mean(recFP, na.rm = TRUE))

ggplot(exploreay) + 
  geom_point(aes(x = air_yards, y = avg_EP), color = 'red') + 
  geom_point(aes(x = air_yards, y = avg_FP), color = 'blue') +
  xlim(-5,45) +
  ylim(0,5)

explore2 <- rushtrainingData %>%
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, rushFP, rushEP, down) %>%
  group_by(yardline_100) %>% 
  summarise(
    avg_EP = mean(rushEP, na.rm = TRUE),
    avg_FP = mean(rushFP, na.rm = TRUE),
    avg_TD = mean(6*touchdown, na.rm = TRUE))

ggplot(explore2) + 
  geom_point(aes(x = yardline_100, y = avg_EP), color = 'red') + 
  geom_point(aes(x = yardline_100, y = avg_FP), color = 'green') +
  geom_point(aes(x = yardline_100, y = avg_TD), color = 'blue')


rushdf2019$rushEP <- predict(lmMod2, rushdf2019)

playerrushEP <- rushdf2019 %>%
  group_by(rusher_player_name) %>%
  summarise(EP = sum(rushEP, na.rm=TRUE),
            FP = format(sum(rushFP), scientific = FALSE),
            diff = EP- as.numeric(FP))

temp <- rushdf2019 %>%
  dplyr::select(rusher_player_name, yardline_100, rushEP, rushFP) %>%
  filter(rusher_player_name == "M.Mack")

