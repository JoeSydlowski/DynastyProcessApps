library(rvest)

#setwd("/srv/data/files/fp-weekly-scrapes-data")

list_of_pages <- c("dynasty-overall", "dynasty-qb", "dynasty-rb", "dynasty-wr",
                   "dynasty-te", "dynasty-k", "dynasty-dst", "rookies",
                   "ppr-cheatsheets", "qb-cheatsheets", "ppr-rb-cheatsheets",
                   "ppr-wr-cheatsheets", "ppr-te-cheatsheets", "k-cheatsheets",
                   "dst-cheatsheets")


for (page in list_of_pages)
{
  url <- paste0("https://www.fantasypros.com/nfl/rankings/",page,".php")
  webpage <- read_html(url)
  df <- webpage %>%
    html_node("table") %>%
    html_table(fill=TRUE) %>%
    .[1:10]
  
  df <- df[!(df$Avg == "" | df$Avg == "&nbsp" | grepl('googletag', df$Avg)), ]
  
  write.csv(df,
            file = paste0(page, "-", format(Sys.Date(), format = "%Y%m%d"),".csv"),
            row.names=FALSE,
            na="")
}
