library(httr)
library(jsonlite)
library(tidyverse)

options(stringsAsFactors = FALSE)

path<-"https://www03.myfantasyleague.com/2019/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1"

mfl<-GET(path) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten=TRUE)%>%
    data.frame()

