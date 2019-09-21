library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)

Sys.setenv(TZ="CST")
todayMonth <- month(today())
todayDay <- day(today())

teamIDs <- read.csv("/srv/shiny-server/DynastyProcess/database-v2/teamIDs.csv")

setwd("/srv/data/files")

if (between(todayMonth,10,12) | (todayMonth == 9 & todayDay >= 10))
{list_of_pages <- c("dynasty-overall", "dynasty-qb", "dynasty-rb", "dynasty-wr",
                    "dynasty-te", "dynasty-k", "dynasty-dst", "rookies",
                    "ros-ppr-overall", "ros-qb", "ros-ppr-rb",
                    "ros-ppr-wr", "ros-ppr-te", "ros-k", "ros-dst",
                    "ros-idp", "ros-dl", "ros-lb", "ros-db",
                    "ppr-flex", "qb", "ppr-rb", "ppr-wr", "ppr-te", "k", "dst", "idp", "dl", "lb", "db")
} else if (between(todayMonth,9,12) | (todayMonth == 8 & todayDay >= 15))
{list_of_pages <- c("dynasty-overall", "dynasty-qb", "dynasty-rb", "dynasty-wr",
                    "dynasty-te", "dynasty-k", "dynasty-dst", "rookies",
                    "ppr-cheatsheets", "qb-cheatsheets", "ppr-rb-cheatsheets",
                    "ppr-wr-cheatsheets", "ppr-te-cheatsheets", "k-cheatsheets", "dst-cheatsheets",
                    "idp-cheatsheets", "dl-cheatsheets", "lb-cheatsheets", "db-cheatsheets",
                    "ppr-flex", "qb", "ppr-rb", "ppr-wr", "ppr-te", "k", "dst", "idp", "dl", "lb", "db")

} else
{list_of_pages <- c("dynasty-overall", "dynasty-qb", "dynasty-rb", "dynasty-wr",
                    "dynasty-te", "dynasty-k", "dynasty-dst", "rookies",
                    "ppr-cheatsheets", "qb-cheatsheets", "ppr-rb-cheatsheets",
                    "ppr-wr-cheatsheets", "ppr-te-cheatsheets", "k-cheatsheets", "dst-cheatsheets",
                    "idp-cheatsheets", "dl-cheatsheets", "lb-cheatsheets", "db-cheatsheets")
}



dfComb <- data.frame()

for (page in list_of_pages)
{ url <- paste0("https://www.fantasypros.com/nfl/rankings/",page,".php")
  webpage <- read_html(url)
  df <- webpage %>%
    html_node("table") %>%
    html_table(fill=TRUE) %>%
    .[4:min(10,ncol(.))]
  
  df2 <- webpage %>%
    html_node("table") %>%
    html_nodes(".player-row")
  
  dfName <- data.frame(
    Player = html_text(html_node(df2,'.full-name')),
    Team = html_text(html_node(df2,'.grey'))
  )
  
  df <- df[!(df$Avg == "" | df$Avg == "&nbsp" | grepl('googletag', df$Avg)), ]
  
  #numCols <- c("Rank","Bye","Best","Worst","Avg","Std Dev","ADP","vs, ADP")
  if(page %in% c('dynasty-overall', 'rookies', 'ppr-cheatsheets', 'ros-ppr-overall', 'ppr-flex', 'idp', 'idp-cheatsheets', 'ros-idp'))
  {
    numCols <- c(4:9)
  } else 
  {numCols <- c(3:8)}
  
  if(nrow(dfName) > 0) {
    df <- cbind(dfName, df)
    df$Page <- page
  } else
  {df <- data.frame()
  numCols <- c()}
  
  

  df[numCols] <- lapply(df[numCols], as.numeric)

  df[grep("vs. ADP", colnames(df))] <- as.character(df[grep("vs. ADP", colnames(df))])

  
  if(!is.null(df$Pos))
  {df$Pos <- substr(df$Pos,1,2)}

  
  if(nrow(df) > 0)
  {
  
  if (grepl("qb",page))
  {df$Pos <- "QB"}
  else if (grepl("rb", page))
  {df$Pos <- "RB"}
  else if (grepl("wr", page))
  {df$Pos <- "WR"}
  else if (grepl("te", page))
  {df$Pos <- "TE"}
  else if (grepl("-k", page))
  {df$Pos <- "K"}
  else if (grepl("k-", page))
  {df$Pos <- "K"}
  else if (page == "k")
  {df$Pos <- "K"}
  else if (grepl("dst", page))
  {df$Pos <- "DST"}
  else if (grepl("dl", page))
  {df$Pos <- "DL"}
  else if (grepl("lb", page))
  {df$Pos <- "LB"}
  else if (grepl("db", page))
  {df$Pos <- "DB"}
  
  df$Pos[df$Pos == "DS"] <- "DST"
  df$Pos[df$Pos == "CB"] <- "DB"
  df$Pos[df$Pos == "DE"] <- "DL"
  df$Pos[df$Pos == "DT"] <- "DL"
  
  df$Pos <- gsub('(^S[0-9]*$)',"DB",df$Pos)
  df[grepl("K", df$Pos), 'Pos'] <- 'K'

  dfComb <- bind_rows(dfComb, df)
  }
  
}

colnames(dfComb)[colnames(dfComb)=="Std Dev"] <- "SD"
colnames(dfComb)[colnames(dfComb)=="Avg"] <- "ECR"

dfComb$Page[dfComb$Page == "dynasty-overall"] <- "do"
dfComb$Page[dfComb$Page %in% c("ppr-cheatsheets", "ros-ppr-overall", 'idp-cheatsheets', 'ros-idp' )] <- "ro"

dfComb$Page[dfComb$Page %in% c("ppr-flex","idp")] <- "wo"

dfComb$Page[dfComb$Page == "rookies"] <- "rook"

dfComb$Page[grep("dynasty", dfComb$Page)] <- "dp"
dfComb$Page[grep("cheatsheets", dfComb$Page)] <- "rp"
dfComb$Page[grep("ros", dfComb$Page)] <- "rp"
dfComb$Page[dfComb$Page %in% c("qb", "ppr-rb", "ppr-wr", "ppr-te", "k", "dst", "idp", "dl", "lb", "db")] <- "wp"


dfTotal <- dfComb[1:10] %>%
  filter(!(Player == 'Mike Davis' & SD == 0.0),
         !(Player == 'Mike Davis' & SD == 0.5),
         !(Player == 'Josh Allen' & SD == 0.0),
         !(Player == 'Michael Thomas' & SD == 0.0),
         !(Player == "Dont'a Hightower" & SD == 0.0),
         !(Player == 'Kirk Cousins' & SD == 0.0),
         !(Player == 'B.J. Hill' & SD == 0.0),
         !(Player == 'Mecole Hardman' & SD == 0.0),
         !(Player == 'Derek Barnett' & SD == 0.0)) %>%
  select(Player, Team, Pos, ECR, Best, Worst, Page, SD) %>%
  group_by(Player) %>%
  gather(variable, value, ECR, SD, Best, Worst) %>%
  unite(temp, Page, variable, sep = '') %>%
  spread(temp, value)

dfTotal$Player <- gsub('(-)|[[:punct:]]|( Jr)|( Sr)|( III)|( II)|( IV)','\\1',dfTotal$Player)

dfTotal$ScrapeDate <- Sys.Date()

dstSplit <- dfTotal %>%
  filter(Pos == "DST") %>%
  separate(Player, c("Player", "Team"), '[ ](?=[^ ]+$)')

dfTotal <- dfTotal %>%
  filter(Pos != "DST")

dfTotal <- bind_rows(dfTotal, dstSplit)

dfTotal <- merge(dfTotal, teamIDs, by.x='Team', by.y='spotrac', all.x = TRUE )

dfTotal$Team <- dfTotal$mfl
dfTotal[c("X","mfl")] <- NULL

dfTotal <- dfTotal %>%
  select(Player,  everything()) %>%
  arrange(roECR)


write.csv(dfTotal,
          file = paste0("/srv/data/files/fantasypros/ecr_", format(Sys.Date(), format = "%Y%m%d"),".csv"),
          row.names=FALSE,
          na="")

system("sudo systemctl restart shiny-server")
system("git add fantasypros/*")
system("git commit -m 'weekly ecr commit'")
#system("git push origin master")
