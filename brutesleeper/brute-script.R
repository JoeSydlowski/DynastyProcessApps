library(jsonlite)
library(tidyverse)

# Script for looping through all of the leagues for a specific user


getuserid <- function(username) {
  fromJSON(paste0("https://api.sleeper.app/v1/user/",username),flatten=TRUE)$user_id
}

getuserleagues <-function(userid){
  leaguelist<-fromJSON(paste0('https://api.sleeper.app/v1/user/',userid,'/leagues/nfl/','2019'))$league_id
  list(league_id=leaguelist)
}

getotherusers <-function(id){
  ids<-fromJSON(paste0('https://api.sleeper.app/v1/league/',id,'/users'))$user_id
  list(idlist=ids)
}


users<-tibble(user='solarpool') %>% 
  mutate(uid=getuserid(user),leagues=getuserleagues(uid)) %>% 
  unnest(leagues) %>% 
  mutate(otherusers=lapply(leagues,getotherusers)) %>% 
  hoist(otherusers,ids="idlist") %>% 
  unnest_longer(ids) %>% 
  filter(distinct(ids))

