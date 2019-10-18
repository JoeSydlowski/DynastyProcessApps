#library(httr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(here)

options(stringsAsFactors = FALSE)

setwd(here())

# Hardcoded data files
teamIDs<-read.csv("teamIDs.csv") #teamdb because am lazy to fix nomenclature
stats2018<-read.csv("stats2018.csv") #stat-set built from NFLScrapR/PFR -> automation a project for 2019 offseason
rookieadp<-read.csv("rookie-adp.csv") #built from mizelle (really, DLF) ADP for rookie pick value analysis

# RASathletic<-read.csv("ras-athletic.csv") #courtesy of @Mathbomb on Twitter
# pfrIDs<-read.csv("PFR-IDs.csv") #originally done with linkify but now grabbing from the URLs

# Replicating the "MFL" query of database builder

mfl<-fromJSON("https://www03.myfantasyleague.com/2019/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1")$players$player %>% 
  select(mfl_id=id,name,position,team,
                  draft_year,stats_global_id,fleaflicker_id,
                  rotoworld_id,nfl_id,espn_id,sportsdata_id,
                  draft_round,draft_pick,height,
                  weight,college,birthdate) %>%
        filter(position %in% c("QB","RB","WR","TE"))%>%
        mutate(birthdate = as.Date(as.POSIXct(as.numeric(birthdate),origin="1970-01-01")),
               age = round((Sys.Date()-birthdate)/365.25,digits=1),sportsdata_id=as.character(sportsdata_id))

# Replicating the "Sleeper" query of my database builder

sleeper<-fromJSON("https://api.sleeper.app/v1/players/nfl") %>% 
  tibble() %>% unnest_wider(1) %>% 
  select(c(sleeper_id="player_id","first_name","last_name", "team","position",
           "fantasy_data_id","rotowire_id","rotoworld_id",
           "sportradar_id","espn_id","gsis_id",
           "stats_id","yahoo_id")) %>% 
  mutate(sportradar_id=as.character(sportradar_id)) %>% 
  filter(position %in% c("QB",'RB','WR','TE'))

sleeper[sleeper=="NULL"]<-""
sleeper[sleeper=="NA"]<-""

# Get Spotrac
spotrac<-read_html("https://www.spotrac.com/nfl/contracts") %>%
  html_node(xpath='/html/body/div[2]/div[2]/div/div/div[1]/div/div[3]/table') %>%
  html_table()%>%
  separate(Player,c("name",NA),sep=' \\(') %>%
  separate(Team,c("team",NA),sep='Signed')

spotrac<-separate(spotrac,name,c("firstname","lastname"),sep=" ", extra="merge") %>% 
  transform(firstname=mapply(sub,lastname,"",firstname,fixed=TRUE)) %>%
  unite("name",firstname,lastname,sep=" ")%>%
  merge(teamIDs,by.x="team",by.y="spotrac",suffixes=c("",".t")) %>%
  select(name,team=mfl,pos=Pos,avg_salary=Average.Salary,fa_yr=Free.Agent) %>%
  filter(pos %in% c("QB","RB","WR","TE"))

spotrac$name<-tolower(gsub("( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)","",spotrac$name))
spotrac$pos<-gsub("FB","RB",spotrac$pos)
spotrac$avg_salary<-as.numeric(gsub("(\\$)|(\\,)","",spotrac$avg_salary))/1000000 %>% 
  round(digits=1)

# Get FantasyPros scrapes

fantasypros<-list.files(path='/srv/data/files/fantasypros',full.names = T) %>% 
  .[length(.)] %>%
  read.csv() %>%
  mutate(mergename=tolower(Player)) %>%
  select(mergename,team=Team,pos=Pos,doECR,dpECR,rpECR)

# get RAS file

ras_mergedmetrics <- read.csv('/srv/data/files/datasets/RAS/MERGED_METRICS.csv') %>% 
  filter(DRAFTYEAR>=2000 & POSITION %in% c('QB','RB','WR','TE'))

ras_menuinfo <- read.csv('/srv/data/files/datasets/RAS/MENU_INFO.csv') %>% 
  filter(DRAFTYEAR>=2000 & POSITION %in% c('QB','RB','WR','TE'))

ras<-ras_mergedmetrics %>% 
  nest_join(ras_menuinfo,by=c('PLAYERID'='PLAYERID')) %>% 
  hoist(y,
        NAME='NAME',
        COLLEGE='COLLEGE',
        ROUND='ROUND',
        PICK='PICK',
        TEAM='TEAM',
        RAS='RAS',
        ATRAS='ATRAS'
        ) %>% 
  select(-y)

# Merge all

database<-left_join(mfl,sleeper,by=c("sportsdata_id"="sportradar_id"),suffix=c("",".y")) %>% 
  select(-ends_with('.y')) %>%
  mutate(name2=name)%>%
  separate(name2,into=c('lname','fname'),sep = ", ") %>%
  mutate(displayname=gsub("( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)","",paste(fname,lname)),
         mergename=tolower(displayname),
         first_name=fname,last_name=lname,pos=position) %>%
  select(-fname,-lname,-position) %>%
  left_join(spotrac,by=c("mergename"="name","team"="team"),suffix=c("",".y")) %>%
  left_join(fantasypros,by=c("mergename"="mergename","team"="team","pos"="pos")) %>% 
  mutate(avg_salary=round(avg_salary,digits=1)) %>% 
  select(-ends_with('.y'))

write.csv(database,file = 'database.csv')
  
