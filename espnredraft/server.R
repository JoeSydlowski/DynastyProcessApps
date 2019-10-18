library(shiny)
library(rvest)
library(dplyr)
library(tidyr)
library(jsonlite)
library(RColorBrewer)
library(DT)

colors<-colorRampPalette(brewer.pal(3,'PuOr'))

rankingdf <- read.csv('https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/fantasypros/ecr_20190913.csv')
rankingdf$Player <- as.character(rankingdf$Player)
rankingdf$Pos <- as.character(rankingdf$Pos)

json_data <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?view=mRoster&view=mTeam&view=mMatchupScore', flatten = TRUE)
json_data2 <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?scoringPeriodId=2&view=mRoster&view=mTeam&view=mMatchupScore', flatten = TRUE)

projections<-read.csv("https://raw.githubusercontent.com/JoeSydlowski/DynastyProcessApps/master/leagueanalyzer/projections.csv",fileEncoding="UTF-8-BOM")
projections$pos <- as.character(projections$pos)

#matchupjson <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?matchupPeriodId=1&view=mMatchup', flatten = TRUE)
#matchupjson1 <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?scoringPeriodId=1&view=mMatchup', flatten = TRUE)
#matchupjson2 <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/805175?scoringPeriodId=2&view=mMatchup', flatten = TRUE)

#playerdf <- fromJSON('http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/players?scoringPeriodId=0&view=players_wl')

shinyServer(function(input, output) {
    
    teamdf <- json_data[["teams"]]
    teamdf$owner <- paste(teamdf$location,teamdf$nickname)

    dfRosters <- data.frame()
    for (i in c(1:length(json_data[["teams"]][["roster.entries"]])))
    {
        dftemp <- json_data[["teams"]][["roster.entries"]][[i]]
        dfRosters <- bind_rows(dfRosters, dftemp)
    }
    
    dfRosters$fullName <- gsub('([.,\'\"])|( Jr)|( Sr)|( III)|( II)|( IV)|( V$)','',dfRosters$`playerPoolEntry.player.fullName`)
    dfRosters$fullName[dfRosters$fullName== "Mitchell Trubisky"] <- "Mitch Trubisky"
    dfRosters$fullName[dfRosters$fullName== "DeVante Parker"] <- "Devante Parker"
    
    output$sched <- renderDT({
        
    sched <- json_data[["schedule"]] %>%
        filter(away.totalPoints > 0) %>%
        mutate(
            winner = if_else(away.totalPoints > home.totalPoints, away.teamId, home.teamId)
        )
    
    schedLong <- bind_rows(sched[c("matchupPeriodId","away.teamId","away.totalPoints","winner")],
                           sched[c("matchupPeriodId","home.teamId","home.totalPoints","winner")]) %>%
        mutate(
            Scores = coalesce(away.totalPoints, home.totalPoints),
            teamID = coalesce(away.teamId, home.teamId),
            win = if_else(teamID == winner, 1, 0)
        ) %>%
        group_by(matchupPeriodId) %>% 
        arrange(Scores, .by_group = TRUE) %>%
        mutate(winProb = (row_number() -1) / 11) %>%
        ungroup() %>% 
        group_by(teamID) %>%
        summarise(
            totalWins = sum(win),
            ExpWins = sum(winProb),
            APperc = mean(winProb),
            Diff = totalWins - ExpWins
        ) %>%
        inner_join(teamdf, by=c("teamID"="id")) %>%
        select(Team = owner, totalWins, APperc, ExpWins, Diff)
    
    df <- datatable(schedLong,
                    rownames=FALSE,
                    options(
                        paging=FALSE,
                        searching=FALSE)) %>%
        formatPercentage('APperc', 1) %>% 
        formatRound(columns=c('ExpWins', 'Diff'), digits=2)
    
    df
    })

    
    merged <- dfRosters %>% 
        inner_join(teamdf, by=c("playerPoolEntry.onTeamId"="id")) %>% 
        #inner_join(playerdf, by=c("playerId"="id")) %>%
        inner_join(rankingdf, by=c("fullName"="Player")) %>%
        left_join(projections,c('Pos'='pos','rpECR'='rank')) %>%
        select(owner, Player = fullName, pos = Pos, roECR, pts)
    
    starters <- merged %>%
        arrange(owner, pos, desc(pts)) %>%
        group_by(owner, pos) %>%  mutate(posrank = rank(desc(pts), ties.method = 'first')) %>%  ungroup() %>%
        mutate(
            starter = case_when(
                pos == "QB" & posrank <= 1 ~ "QB",
                pos == "RB" & posrank <= 2 ~ "RB",
                pos == "WR" & posrank <= 2 ~ "WR",
                pos == "TE" & posrank <= 1 ~ "TE"
            ),
            flex_elig = case_when(pos != "QB" & is.na(starter) ~ 1)) %>%
        group_by(owner, flex_elig) %>%
        mutate(
            flexrank = case_when(flex_elig==1 ~ rank(desc(pts), ties.method = 'first')),
            flex = case_when(flexrank <= 2 ~ "FLEX"),
            lineup = case_when(!is.na(starter) ~ paste0(starter,posrank),
                               flex == "FLEX" ~ paste0(flex,flexrank)),
            sflex_elig = case_when(is.na(lineup) & pos %in% c('QB','RB','WR','TE')~1)
        )%>%
        ungroup() %>%
        group_by(owner, sflex_elig) %>%
        mutate(
            sflexrank=case_when(sflex_elig==1 ~ rank(desc(pts),ties.method='first')),
            sflex = case_when(sflexrank<=1~"SFLEX"),
            lineup = case_when(!is.na(lineup)~lineup,
                               sflex == "SFLEX" ~ paste0(sflex,sflexrank))
        )%>%
        ungroup()
    
    
    pivot_rpos <- starters %>%
        mutate_at(vars(lineup),  ~ replace(., is.na(.), "Bench")) %>%
        group_by(owner, lineup) %>%
        summarize(pts = sum(pts, na.rm = TRUE)) %>% ungroup() %>%
        mutate(bench = case_when(lineup == "Bench" ~ 1)) %>%
        group_by(owner, bench) %>% mutate(total = sum(pts, na.rm = TRUE)) %>%  ungroup() %>%
        spread(lineup, pts) %>%
        gather(lineup, pts, 3:ncol(.)) %>%
        drop_na(pts) %>%
        mutate(
            lineup = case_when(
                bench == 1 & lineup == "total" ~ "NA",
                lineup == "total" ~ "Starters Total",
                bench == 1 ~ "Bench",
                TRUE ~ lineup)
        )%>%
        transmute(owner = owner, lineup = lineup, pts=pts)  %>%
        spread(lineup,pts) %>%
        select(owner, starts_with("QB"), starts_with("SFLEX"), starts_with("RB"), starts_with("WR"), starts_with("TE"), starts_with("FLEX"), starters=`Starters Total`, bench=Bench)%>%
        mutate(team=starters+bench)%>%
        arrange(desc(starters))%>%
        mutate_all(~replace(.,is.na(.),0))
    
    brks_rpos<-function(colnum){
        breakvalue<-quantile(range(pivot_rpos[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
        return(breakvalue)
    }
    
    dt_rpos<-datatable(pivot_rpos,
                       rownames=FALSE,
                       options(
                           paging=FALSE,
                           searching=FALSE
                       ))
    
    for(i in 2:ncol(pivot_rpos)){
        dt_rpos<-dt_rpos%>%
            formatStyle(i, backgroundColor = styleInterval(brks_rpos(i),colors(length(brks_rpos(i))+1)))
    }
    
    output$pivot <- renderDT({
        dt_rpos
    })

})
