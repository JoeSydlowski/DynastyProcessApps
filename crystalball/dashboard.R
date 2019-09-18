library(jsonlite)
library(tidyr)
library(dplyr)
library(DT)
library(RColorBrewer)
library(httr)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin="blue",
  dashboardHeader(title = "DynastyProcess.com"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MFL", tabName = "mfl", icon = icon("quidditch")),
      menuItem("Sleeper", tabName = "sleeper", icon = icon("magic"))
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "mfl",
            fluidRow(
              box(width=12,
              titlePanel("DynastyProcess.com: Crystal Ball App"),
              includeMarkdown("about.md"))),
            fluidRow(column(6,offset=3,
              box(width=12,title="MFL League ID",
                  textInput('mflid',NULL,value="54040",placeholder="Should be five digits!"),
                  actionButton('mflloaddata','Load'))#,
              # box(width=6,title="Authentication",
              #     textInput('mflusername',NULL,placeholder="Username"),
              #     passwordInput('mflpassword',NULL,placeholder="Password"),
              #     actionButton('mfllogin',"Login"),
              #     textOutput('authcode')
              #     )
              )),
            fluidRow(
              box(title="Season Projections",width=12,
                  DTOutput('mflsummarytbl')
                  )),
            fluidRow(box(width=12,title="Schedule",DTOutput('mfldetailstbl')))
    ),
    tabItem(tabName = "sleeper",
            fluidRow(
              box(width=12,
                  titlePanel("DynastyProcess.com: Crystal Ball App"),
                  includeMarkdown("about.md"))),
            fluidRow(
              box(width=4, title="Sleeper Username",
                  textInput('sleeperusername',NULL,value="solarpool",placeholder = "username"),
                  actionButton('sleeperloaduser','Load!')),
              box(width=8, title="League List",
                  DTOutput('sleeperleaguelist'),
                  textOutput('selectedleague'))),
            fluidRow(
              box(title="Season Projections",width=12,
                  DTOutput('sleepersummarytbl')
              )),
            fluidRow(box(width=12,title="Schedule",DTOutput('sleeperdetailstbl')))
            
            
    )
  )
  )
)

server <- function(input, output, session) {
  # MFL code
  
  # for authentication somewhere in the future, needs rebuilding m_franchises/m_standings/m_schedule to accomplish
  # m_cookie<-eventReactive(input$mfllogin,{GET(paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",input$mflusername,"&PASSWORD=",URLencode(input$mflpassword,reserved=TRUE),"&XML=1"))$cookies$value %>% 
  #     URLencode(reserved=TRUE)})

  m_franchises<-eventReactive(input$mflloaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=league&L=",input$mflid,"&APIKEY=&JSON=1"))$league$franchises$franchise %>%
      select(ownerid=id,owner=name)})
  
  m_standings<-eventReactive(input$mflloaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=leagueStandings&L=",input$mflid,"&APIKEY=&JSON=1"))$leagueStandings$franchise %>% 
      select(ownerid=id,pointsfor=pf,potentialpoints=pp,starts_with('h2h'),starts_with('all_play')) %>%
      mutate_at(vars(starts_with('all_play')),as.numeric) %>%
      mutate(record=paste0(h2hw,"-",h2ht,"-",h2hl),
             allplaypct=all_play_w/(all_play_w+all_play_t+all_play_l)) %>%
      select(-starts_with('all_play')) %>%
      inner_join(m_franchises(),by=c('ownerid'='ownerid'))})
  
  
  m_schedule <-eventReactive(input$mflloaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=schedule&L=",input$mflid,"&APIKEY=&JSON=1"))$schedule$weeklySchedule %>% 
      unnest(matchup) %>% 
      unnest_wider(franchise) %>%
      filter(result=='NA') %>% 
      select(id,week) %>%
      hoist(id,team=1,opp=2)})
  
  m_fullschedule <- eventReactive(input$mflloaddata,{m_schedule() %>%
      select(week,team=opp,opp=team) %>%
      bind_rows(m_schedule()) %>%
      mutate(week=as.numeric(week)) %>% 
      arrange(week,team) %>%
      nest_join(m_standings(), by=c("team"="ownerid"),name='teaminfo') %>%
      nest_join(m_standings(),by=c("opp"="ownerid"),name='oppinfo') %>%
      hoist(teaminfo,team_name="owner",team_pct="allplaypct") %>%
      hoist(oppinfo,opp_name="owner",opp_pct="allplaypct") %>%
      select(-teaminfo,-oppinfo,-opp) %>%
      mutate(
        team_pct = round(team_pct,digits=3),
        opp_pct = round(opp_pct, digits=3),
        win_prob=round(team_pct/(team_pct+opp_pct),digits=3))})
  
  m_expectedwins<-eventReactive(input$mflloaddata,{m_fullschedule() %>%
      group_by(team,team_name) %>%
      summarize(ewins=sum(win_prob,na.rm=TRUE),
                elosses=n()-ewins) %>%
      ungroup() %>%
      nest_join(m_standings(),by=c('team'='ownerid'),name="m_standings") %>%
      hoist(m_standings,h2hw="h2hw",h2ht="h2ht",h2hl="h2hl",`AllPlay%`="allplaypct") %>%
      mutate_at(vars("h2hw","h2ht","h2hl"),as.numeric) %>%
      mutate(TotalWins=ewins+h2hw, TotalLosses=elosses+h2hl,`AllPlay%`=round(`AllPlay%`,digits=3)) %>%
      select(Team = team_name,`AllPlay%`,Wins=h2hw,Losses=h2hl,rosWins=ewins,rosLosses=elosses,TotalWins,TotalLosses) %>%
      arrange(desc(TotalWins))})
  
  m_fspivot<-eventReactive(input$mflloaddata,{m_fullschedule() %>%
      mutate(weekname=week) %>%
      select(weekname,team_name,win_prob) %>%
      pivot_wider(names_from=weekname,values_from = win_prob) %>%
      rename(Team=team_name)})
  
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))
  
  
  brks<-function(tb,colnum){
    breakvalue<-quantile(range(tb[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
    return(breakvalue)
  }
  
  
  output$mflsummarytbl<-renderDT({
    mfl_sumtbl<-datatable(m_expectedwins(), rownames=FALSE, options=list(scrollX=TRUE,pageLength=25))
    for(colnum in c(2,3,5,7)){
      mfl_sumtbl<-mfl_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(m_expectedwins(),colnum),colourlist(20)))%>%
        formatPercentage(2,1)
    }
    for(colnum in c(4,6,8)){
      mfl_sumtbl<-mfl_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(m_expectedwins(),colnum),rev(colourlist(20))))
    }
    mfl_sumtbl
  })
  
  output$mfldetailstbl<-
    renderDT({
      datatable(m_fspivot(),rownames=FALSE, options=list(scrollX=TRUE,pageLength=50)) %>% 
        formatStyle(-1,backgroundColor = styleInterval(brks(m_fspivot(),-1),colourlist(20))) %>%
        formatPercentage(-1,1)
    })
  
  #Sleeper chunks start here!
  
  ## select sleeper league
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  sleeperuserid<-eventReactive(input$sleeperloaduser,fromJSON(paste0("https://api.sleeper.app/v1/user/",input$sleeperusername),flatten=TRUE)$user_id)
  
  userleagues<-eventReactive(input$sleeperloaduser,{fromJSON(paste0("https://api.sleeper.app/v1/user/",sleeperuserid(),"/leagues/nfl/",'2019'))%>%
      select(Name=name,League_ID=league_id) %>%
      mutate(LeagueSelect=shinyInput(actionButton,nrow(.),'button_',label="Select",onclick='Shiny.onInputChange(\"select_button\",  this.id)'))
    })
  
  stringsAsFactors=FALSE
  
  output$sleeperleaguelist<-renderDT(userleagues(),rownames=FALSE,escape=FALSE,selection = 'none',options=list(scrollX=TRUE,pageLength=10))

  s <- reactiveValues(sleeperid = '')
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    s$sleeperid <<- as.character(userleagues()$League_ID[selectedRow])
  })
  
  output$selectedleague<-renderText({s$sleeperid})

  
  ## the rest of the chunks

  s_playoffweekstart<-reactive({fromJSON(paste0("https://api.sleeper.app/v1/league/",s$sleeperid))$settings$playoff_week_start-1})
  
  s_matchups<-eventReactive(input$select_button,{tibble(week=c(1:playoffweekstart))})
  
  s_getmatchups<-function(i,LID){
    fromJSON(paste0("https://api.sleeper.app/v1/league/",LID,"/matchups/",i)) %>%
    select(roster_id,points,matchup_id)
  }
  
  s_matchupdata<-eventReactive(input$select_button,{s_matchups()%>%
      mutate(data=lapply(week,getmatchups,leagueID)) %>% 
      hoist(data,roster_id='roster_id',points='points',matchup_id='matchup_id') %>% 
      unnest(roster_id,points,matchup_id) %>%
      select(-data)})
  
    s_teamlist<-eventReactive(input$select_button,{fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/rosters"))%>%
      select(owner_id,roster_id)})
    
    s_users<- eventReactive(input$select_button,{fromJSON(paste0("https://api.sleeper.app/v1/league/",as.character(leagueID),"/users"),flatten=TRUE) %>%
      select(owner=display_name,owner_id=user_id,teamname=metadata.team_name)%>%
      inner_join(s_teamlist(),by="owner_id") %>% 
      mutate_all(as.character)})
    
    s_matchups1<-eventReactive(input$select_button,{s_matchupdata() %>% 
      select(opp=roster_id,wk=week,opp_pts=points,m_id=matchup_id) %>%
      mutate(opp=as.character(opp),opp_pts=as.character(opp_pts)) %>%
      unite("merge",opp,opp_pts)})
    
    s_schedule<- eventReactive(input$select_button,{s_matchupdata() %>% 
      nest_join(s_matchups1(),by=c('week'='wk','matchup_id'='m_id'),name='matchups1') %>% 
      unnest_wider(matchups1) %>%
      unnest_longer(merge) %>%
      separate(merge,into=c('opp','opp_pts'),sep="_") %>%
      mutate(opp_pts=round(as.numeric(opp_pts),digits=2)) %>% 
      filter(roster_id!=opp) %>%
      mutate(wins=if_else(points>opp_pts,1,0)) %>%
      group_by(week)%>%
      mutate(allplaywins=rank(points)-1,gms=n()-1)%>%
      ungroup()})
    
    s_standings<-eventReactive(input$select_button,{s_schedule() %>%
      filter(!is.na(points)) %>%
      group_by(roster_id)%>%
      summarize(wins=sum(wins),allplaywins=sum(allplaywins),weeks=n(),allplaygms=sum(gms)) %>%
      ungroup() %>% 
      mutate(losses=weeks-wins,allplaypct=round(allplaywins/allplaygms,digits=3),allplaylosses=allplaygms-allplaywins,roster_id=as.character(roster_id))})
    
    s_fullschedule<- eventReactive(input$select_button,{s_schedule() %>%
      filter(is.na(points)) %>% 
      select(roster_id,week,opp)%>%
      mutate(roster_id=as.character(roster_id)) %>% 
      nest_join(s_standings(),by='roster_id',name='teaminfo') %>%
      nest_join(s_standings(),by=c('opp'='roster_id'),name='oppinfo')%>%
      hoist(teaminfo,team_pct='allplaypct')%>%
      hoist(oppinfo,opp_pct='allplaypct')%>%
      mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3)) %>%
      select(roster_id,week,opp,team_pct,opp_pct,win_prob)})
    
    s_summary<-eventReactive(input$select_button,{s_fullschedule() %>% 
      group_by(roster_id) %>% 
      summarize(rosWins=sum(win_prob),rosGms=n()) %>%
      ungroup() %>% 
      left_join(s_standings(),by='roster_id') %>% 
      left_join(s_users(),by='roster_id') %>%
      mutate(rosLosses=rosGms-rosWins) %>% 
      select(Team=owner,`AllPlay%`=allplaypct,Wins=wins,Losses=losses,rosWins,rosLosses) %>% 
      mutate(TotalWins=Wins+rosWins,TotalLosses=Losses+rosLosses) %>% 
      arrange(desc(TotalWins))})
    
    s_fspivot<-eventReactive(input$select_button,{s_fullschedule() %>% 
      select(roster_id,week,win_prob) %>% 
      nest_join(s_users(),by='roster_id',name='users') %>% 
      hoist(users,Team='owner') %>% 
      select(-roster_id,-users) %>% 
      pivot_wider(names_from = week, values_from=win_prob, names_prefix = "Week")})
    

  
  output$sleepersummarytbl<-renderDT({
    req(input$select_button)
    s_sumtbl<-datatable(s_summary(), rownames=FALSE, options=list(scrollX=TRUE,pageLength=25))
    for(colnum in c(2,3,5,7)){
      s_sumtbl<-s_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(s_summary(),colnum),colourlist(20)))%>%
        formatPercentage(2,1)
    }
    for(colnum in c(4,6,8)){
      s_sumtbl<-s_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(s_summary(),colnum),rev(colourlist(20))))
    }
    s_sumtbl
  })
  
  output$sleeperdetailstbl<-
    renderDT({
      datatable(s_fspivot(),rownames=FALSE, options=list(scrollX=TRUE,pageLength=50)) %>% 
        formatStyle(-1,backgroundColor = styleInterval(brks(s_fspivot(),-1),colourlist(20))) %>%
        formatPercentage(-1,1)
    })
  
  
  
  
  
} #end of server segment

shinyApp(ui, server)