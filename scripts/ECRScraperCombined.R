library(rvest)
library(dplyr)
library(tidyr)

#setwd("/srv/data/files/fp-weekly-scrapes-data")

list_of_pages <- c("dynasty-overall", "dynasty-qb", "dynasty-rb", "dynasty-wr",
                   "dynasty-te", "dynasty-k", "dynasty-dst", "rookies",
                   "ppr-cheatsheets", "qb-cheatsheets", "ppr-rb-cheatsheets",
                   "ppr-wr-cheatsheets", "ppr-te-cheatsheets", "k-cheatsheets",
                   "dst-cheatsheets")

dfComb <- data.frame()

for (page in list_of_pages)
{
  url <- paste0("https://www.fantasypros.com/nfl/rankings/",page,".php")
  webpage <- read_html(url)
  df <- webpage %>%
    html_node("table") %>%
    html_table(fill=TRUE) %>%
    .[4:10]
  
  df2 <- webpage %>%
    html_node("table") %>%
    html_nodes(".player-row")
  
  dfName <- data.frame(
    Name = html_text(html_node(df2,'.full-name')),
    Team = html_text(html_node(df2,'.grey'))
  )
  
  df <- df[!(df$Avg == "" | df$Avg == "&nbsp" | grepl('googletag', df$Avg)), ]
  
  df <- cbind(dfName, df)
  
  #numCols <- c("Rank","Bye","Best","Worst","Avg","Std Dev","ADP","vs, ADP")
  if(page == 'dynasty-overall')
    {numCols <- c(4:9)}
  else if(page == 'rookies')
  {numCols <- c(4:9)}
  else if(page == 'ppr-cheatsheets')
  {numCols <- c(4:9)}
  else {numCols <- c(3:8)}
  
  df[numCols] <- lapply(df[numCols], as.numeric)
  
  df$Page <- page
  
  if(!is.null(df$Pos))
  {df$Pos <- substr(df$Pos,1,2)}
  
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
  else if (grepl("dst", page))
  {df$Pos <- "DST"}
  
  df$Pos <- gsub("DS","DST",df$Pos)
  df[grepl("K", df$Pos), 'Pos'] <- 'K'
  
  dfComb <- bind_rows(dfComb, df)
}

colnames(dfComb)[colnames(dfComb)=="Std Dev"] <- "StDev"

dfComb$Page[dfComb$Page == "dynasty-overall"] <- "dynoECR"
dfComb$Page[dfComb$Page == "ppr-cheatsheets"] <- "redoECR"
dfComb$Page[dfComb$Page == "rookies"] <- "rookECR"

dfComb$Page[grep("dynasty", dfComb$Page)] <- "dynpECR"
dfComb$Page[grep("cheatsheets", dfComb$Page)] <- "redpECR"

dfTotal <- dfComb %>%
  select(Name, Team, Pos, Avg, Page) %>%
  group_by(Name) %>%
  spread(Page, Avg)

dfTotal2 <- dfComb %>%
  select(Name, Team, Pos, Avg, Best, Worst, Page, StDev) %>%
  group_by(Name) %>%
  gather(variable, value, Avg, Best, Worst, StDev) %>%
  unite(temp, variable, Page) %>%
  spread(temp, value)

#write.csv(df,
#          file = paste0(page, "-", format(Sys.Date(), format = "%Y%m%d"),".csv"),
#          row.names=FALSE,
#          na="")