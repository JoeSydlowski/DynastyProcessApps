library(tidyverse)
library(dplyr)
library(nflscrapR)
library(ggplot2)
library(lubridate)
#library(stargazer)
library(MASS)
library(car)

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
         play_type %in% c("run"),
         two_point_attempt == 0) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         rushFP = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
         rushFP1D = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + 0.5*first_down_rush,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         run_gap2 = ifelse((play_type == "run" & is.na(run_gap)), "center", as.character(run_gap))
  )

recdf <- pbp %>% 
  filter(!is.na(epa),
         play_type %in% c("pass"),
         sack == 0,
         two_point_attempt == 0) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         recFP = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         recFP1D = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass+ 0.5*first_down_pass,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         abs_air_yards = abs(air_yards)
  )

recTDMod <- glm(pass_touchdown ~ yardlinesq + yardline_100 + abs_air_yards + pass_location, data=recdf, family=binomial(link="logit"))
recYDMod <- lm(yards_gained ~ logyardline + yardlinesq + abs_air_yards + pass_location + shotgun, data=recdf)
recMod <- glm(complete_pass ~ logyardline + yardlinesq + abs_air_yards + pass_location + shotgun, data=recdf, family=binomial(link="logit"))
rec1DMod <- glm(first_down_pass ~ logyardline + yardline_100 + abs_air_yards + pass_location + shotgun + ydstogo + factor(down), data=recdf, family=binomial(link="logit"))

#stepAIC(rec1dMod)
#vif(rec1dMod)
#stargazer(rec1dMod, type = "text")
#pR2(rec1dMod)

recdf$eRecTD <- plogis(predict(recTDMod, recdf))
recdf$eRecYD <- predict(recYDMod, recdf)
recdf$eRec <- plogis(predict(recMod, recdf))
recdf$eRec1D <- plogis(predict(rec1DMod, recdf))

recFPMod <- lm(recFP ~ eRecTD + eRecYD + eRec , data=recdf)
recFP1DMod <- lm(recFP1D ~ eRecTD + eRecYD + eRec1D, data=recdf)
#stepAIC(recFPMod)
#stargazer(recFPMod, type = "text")
recdf$eRecFP <- predict(recFPMod, recdf)

#Concordance(recdf$recFP, recdf$eFP)
#Concordance(recdf$yards_gained, recdf$eYD)


rushTDMod <- glm(rush_touchdown ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap2 , data=rushdf, family=binomial(link="logit"))
#rushFBMod <- glm(fumble ~ yardline_100 + factor(down) + run_gap2 , data=rushdf, family=binomial(link="logit"))
rushYDMod <- lm(yards_gained ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap2 , data=rushdf)
rush1DMod <- glm(first_down_rush ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap2 + shotgun + ydstogo, data=rushdf, family=binomial(link="logit"))

#stepAIC(rush1dMod)
#vif(rush1dMod)
#stargazer(rush1dMod, type = "text")
#pR2(rush1dMod)

rushdf$eRushTD <- plogis(predict(rushTDMod, rushdf))
#rushdf$eFB <- plogis(predict(rushFBMod, rushdf))
rushdf$eRushYD <- predict(rushYDMod, rushdf)
rushdf$eRush1D <- plogis(predict(rush1DMod, rushdf))

rushFPMod <- lm(rushFP ~ eRushTD + eRushYD, data=rushdf)
rushFP1DMod <- lm(rushFP1D ~ eRushTD + eRushYD + eRush1D, data=rushdf)

#stargazer(rushFP1DMod, type = "text")
#Concordance(rushdf$rush_touchdown, rushdf$eTD)
#Concordance(rushdf$yards_gained, rushdf$eYD)
rushdf$eRushFP <- predict(rushFPMod, rushdf)
rushdf$eRushFP1d <- predict(rushFP1DMod, rushdf)

#df <- bind_rows(recdf, rushdf)

recTDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recYDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rec1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recFPMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recFP1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

rushTDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushYDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rush1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushFPMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushFP1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

save(recTDMod, recYDMod, recMod, rec1DMod, recFPMod, recFP1DMod,
     rushTDMod, rushYDMod, rush1DMod, rushFPMod, rushFP1DMod, file = "models.rda")

dfsum <- df %>%
  group_by(game_id, drive) %>%
  summarise(eTD = sum(eTD, na.rm=TRUE),
            eYD = sum(eYD, na.rm=TRUE),
            maxYds = max(yardline_100)) %>%
  filter(eYD > maxYds)



recMod <- lm(recFP ~ logyardline + yardline_100 + factor(down)
             + abs_air_yards + shotgun  + pass_location, data=rectrainingData)  # build the model
stepAIC(recMod)
stargazer(recMod, type = "text")

rushMod <- lm(rushFP ~ logyardline + yardlinesq + yardline_100 + factor(down)
              + shotgun + run_gap2 , data=rushtrainingData)  # build the model
stepAIC(rushMod)
stargazer(rushMod, type = "text")

save(recMod, rushMod, file = "models.rda")


rectestData$recEP <- predict(lmMod, rectestData)
rushtestData$rushEP <- predict(lmMod2, rushtestData)

sqrt(mean((rectestData$recFP - rectestData$recEP) ^ 2 , na.rm=TRUE))
sqrt(mean((rushtestData$rushFP - rushtestData$rushEP) ^ 2 , na.rm=TRUE))

ggplot(rectestData) + 
  geom_point(aes(x = recEP, y = recFP), color = 'red')


ggplot(rushdf) + 
  geom_point(aes(x = yardline_100, y = first_down_rush), color = 'red')

ggplot(rushtestData) + 
  geom_point(aes(x = rushEP, y = rushFP), color = 'red')


rectrainingData$recEP <- predict(lmMod, rectrainingData)
rushtrainingData$rushEP <- predict(lmMod2, rushtrainingData)


explore <- recdf %>%
  
  explore <- rectrainingData %>%
  
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, recFP, recEP) %>%
  group_by(yardline_100) %>% 
  summarise(
    #avg_EP = mean(recEP, na.rm = TRUE),
    #avg_FP = mean(recFP, na.rm = TRUE),
    avg_1d = mean(first_down_pass, na.rm = TRUE))

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_1d), color = 'red') + 
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


recdf2019$recEP <- predict(lmMod, recdf2019)
rushdf2019$rushEP <- predict(lmMod2, rushdf2019)

teamrecEP <- recdf2019 %>%
  group_by(posteam) %>%
  summarise(TeamTargets = sum(pass_attempt),
            TeamCatches = sum(complete_pass),
            TeamAYs = sum(abs_air_yards, na.rm=TRUE),
            TeamRecEP = sum(recEP, na.rm=TRUE)
  )

playerrushEP <- rushdf2019 %>%
  group_by(rusher_player_name) %>%
  summarise(rushEP = sum(rushEP, na.rm=TRUE),
            rushFP = format(sum(rushFP), scientific = FALSE),
            diff = format(as.numeric(rushFP) - rushEP, nsmall=1),
            Rushes = sum(rush_attempt))

playerrecEP <- recdf2019 %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(recEP = sum(recEP, na.rm=TRUE),
            recFP = format(sum(recFP), scientific = FALSE),
            diff = format(as.numeric(recFP) - recEP, nsmall=1),
            Targets = sum(pass_attempt),
            Catches = sum(complete_pass),
            AYs = sum(air_yards),
            recYds = sum(yards_gained),
            aDOT = mean(air_yards)
  ) %>%
  inner_join(teamrecEP, by = "posteam") %>%
  mutate(AYshare = AYs / TeamAYs,
         TargetShare = Targets / TeamTargets,
         WOPR = 1.4*TargetShare + 0.7*AYshare,
         RACR = recYds / AYs,
         YPTPA = recYds / TeamTargets) %>%
  dplyr::select(receiver_player_name, posteam, recEP, recFP, diff, Targets, Catches, recYds, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)




temp <- rushdf2019 %>%
  dplyr::select(rusher_player_name, yardline_100, rushEP, rushFP) %>%
  filter(rusher_player_name == "M.Mack")

temp2 <- recdf2019 %>%
  dplyr::select(receiver_player_name, yardline_100, air_yards, recEP, recFP) %>%
  filter(receiver_player_name == "T.Kelce")

temp2 <- recdf2019 %>%
  #dplyr::select(receiver_player_name, yardline_100, recEP, rechFP) %>%
  filter(is.na(receiver_player_name) & sack != 1)


temp2 <- recdf %>%
  #dplyr::select(receiver_player_name, yardline_100, recEP, rechFP) %>%
  filter( air_yards <= -15)

temp2 <- df2019 %>%
  group_by(pass_defense_1_player_name) %>%
  summarise(n())
filter( air_yards <= -15)