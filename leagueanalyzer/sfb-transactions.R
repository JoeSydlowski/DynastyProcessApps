library(tidyverse)
library(jsonlite)
library(rvest)
library(DT)
library(ggplot2)
library(ggridges)

leaguesearch<-fromJSON("http://www73.myfantasyleague.com/2019/export?TYPE=leagueSearch&SEARCH=scottfishbowl9&JSON=1")$leagues$league %>%
  select(leaguename=name,leagueid=id)

database<-read.csv('database.csv')%>%
  select(mfl_id,mergename,pos,team,age)%>%
  mutate(mfl_id=as.character(mfl_id))

owners<-data_frame()

for(i in leaguesearch$leagueid){
  lgname<-fromJSON(paste0('http://www73.myfantasyleague.com/2019/export?TYPE=league&L=',i,'&APIKEY=&JSON=1'))$league$name
  
  div<-fromJSON(paste0('http://www73.myfantasyleague.com/2019/export?TYPE=league&L=',i,'&APIKEY=&JSON=1'))$league$divisions$division %>%
    select(divisionname=name,divisionid=id)
  
  df<-fromJSON(paste0('http://www73.myfantasyleague.com/2019/export?TYPE=league&L=',i,'&APIKEY=&JSON=1'))$league$franchises$franchise %>%
    select(divisionid=division,franchiseid=id,owner=name,bbidAvailableBalance) %>%
    mutate(leagueid=i,leaguename=lgname)%>%
    inner_join(div,by=c('divisionid'='divisionid')) %>%
    select(leagueid,divisionid,franchiseid,leaguename,divisionname,owner,bbidAvailableBalance)
  
owners<-bind_rows(owners,df) 
  
rm(div,df,lgname,i)
}

  
waivers<-data_frame()


for(i in leaguesearch$leagueid){
    
    df<-fromJSON(paste0('http://www73.myfantasyleague.com/2019/export?TYPE=transactions&L=',i,
                        '&APIKEY=&W=&TRANS_TYPE=BBID_WAIVER&FRANCHISE=&DAYS=&COUNT=&JSON=1'))$transactions$transaction %>%
      mutate(leagueid=i)
    
    waivers<-bind_rows(waivers,df)
    
    rm(df,i)  
    
}
  
waiverscleanup<-waivers%>%
    separate(transaction,into=c("added","bid","dropped"),sep="\\|")%>%
    separate(added,into=c("added",NA),sep="\\,")%>%
    separate(dropped,into=c("dropped",NA),sep="\\,")%>%
    mutate(bid=as.numeric(bid))%>%
    inner_join(database,by=c("added"='mfl_id'))%>%
    inner_join(owners,by=c('leagueid'='leagueid','franchise'='franchiseid'))%>%
    select(leaguename,divisionname,owner,mergename,pos,team,bid)
  
dt_raw<-datatable(waiverscleanup,
                    rownames=FALSE,
                    filter="top")

ridges<-waiverscleanup %>% ggplot(mapping = aes(x=bid, y=mergename))+geom_density_ridges2()

ridges
