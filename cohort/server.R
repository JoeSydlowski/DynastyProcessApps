library(curl)
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(shinythemes)


x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess-private/master/datasets/ffstats-feb2019/QBdata.csv?token=AtNYwWF-d4Kr_8OGaqPbzrXLhVrwPa_yks5caP09wA%3D%3D"))

cols <- c("Player", "Season", "Age", "Overall", "Positional.4")
y <- x[cols]

shinyServer(function(input, output) {
  
  df1 <- reactive({
    req(input$selected)
    
    y_norm <- y %>% mutate_at(funs(scale(.) %>% as.vector), .vars=vars(3:5))
    rownames(y_norm) <- paste(y_norm$Player, y_norm$Season, sep = "/")
    y_norm <- y_norm[,3:5]
    
    df <- as.data.frame(as.matrix(dist(y_norm[])))
    
    playername <- paste(input$selected, 2, sep = "/")
    
    newdata <- df[order(df[playername,]),]
    
    newdata <- rownames_to_column(newdata, "Name")
    
    cols <- c("Name", playername)
    
    newdata <- newdata[cols]
    
    newdata <- newdata[1:6,]
    
    newdata <- separate(newdata, col = Name, into = c("Name","MatchSeason"), sep = "/")
    
    merge <- left_join(newdata, x, by = c("Name"="Player"))
    
    merge <- merge[c("Name", "Season", "Positional.4")]
    
    merge$Season <- sprintf("%02d", merge$Season)
    
    names(merge)[names(merge) == 'Positional.4'] <- 'PositionalFinish'
    
    r <- reshape(merge, idvar = "Name", timevar = "Season", direction = "wide")
    
    r <- r[ , order(names(r))]
    
    newdata <- newdata[c("Name", "MatchSeason")]
    
    r <- left_join(r, newdata, by = "Name")
    
  })
    
  output$results <- renderDT({
    
    df1()
    
  })
  

})
