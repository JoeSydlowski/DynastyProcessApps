library(curl)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(shinythemes)

#x <- read.csv("C:/Users/syd23/Documents/Fantasy Football/Excel Sheets/FFStatistics/qbdata.csv")
x <- read.csv("qbdata.csv")

cols <- c("Player", "Year", "Season", "Age", "Overall", "PosRank.4pt.TD.")
y <- x[cols]


shinyServer(function(input, output) {
  
  season <- reactive({
    y$Season[which(y$Player == input$selected & y$Year == 2018)]
  })
  
  
  df1 <- reactive({
    req(input$selected)
    
    y_norm <- y %>% mutate_at(funs(scale(.) %>% as.vector), .vars=vars(4:6))
    rownames(y_norm) <- paste(y_norm$Player, y_norm$Season, sep = "/")
    y_norm <- y_norm[,4:6]
    
    df <- as.data.frame(as.matrix(dist(y_norm[])))
    
    playername <- paste(input$selected, season(), sep = "/")
    
    #playername <- "Philip Rivers/15"

    newdata <- df[order(df[playername,]),]
    
    newdata <- rownames_to_column(newdata, "Name")
    
    cols <- c("Name", playername)
    
    newdata <- newdata[cols]
    
    newdata <- newdata[1:5,]
    
    newdata <- separate(newdata, col = Name, into = c("Name","MatchSeason"), sep = "/")
    
    newdata$MatchSeason <- as.numeric(as.character(newdata$MatchSeason))
    
    merge <- left_join(newdata, x, by = c("Name"="Player"))
    
    merge <- merge[c("Name", "Season", "PosRank.4pt.TD.", "MatchSeason")]
    
    merge <- merge[merge$Season >= merge$MatchSeason,]

     merge <- merge %>%
       group_by(Name) %>%
       arrange(Name, Season) %>%
       mutate(Year = row_number()-1) %>%
       ungroup()
    
    #merge$Season <- sprintf("%02d", merge$Season)
    
    names(merge)[names(merge) == 'PosRank.4pt.TD.'] <- 'PositionalFinish'
    
    merge <- merge[c("Name", "Year", "PositionalFinish")]
    
    r <- spread(merge, key = Year, value = PositionalFinish, sep = "_")
    
    #r <- r[ , order(names(r))]
    
    #newdata <- newdata[c("Name", "MatchSeason")]
    
    #r <- left_join(r, newdata, by = "Name")
    
  })
    
  output$results <- renderDT({
    
    df1()
    
  })
  

})
