library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(DT)
library(RColorBrewer)
library(httr)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin="blue", title="DynastyProcess Apps: Crystal Ball",
  dashboardHeader(title = a(href="https://dynastyprocess.com",img(src = "logo-horizontal.png",width='100%')),titleWidth = 250),
  dashboardSidebar(width = 250,
    sidebarMenu(
      menuItem("MFL", tabName = "mfl", icon = icon("quidditch")),
      menuItem("Sleeper", tabName = "sleeper", icon = icon("bed")),
      menuItem("ESPN", tabName = 'espn', icon=icon('tv'))
    ),
    sidebarMenu(
      menuItem("More from DynastyProcess:", icon=icon("rocket"),
               menuSubItem("Calculator",icon=icon('calculator'),href="https://apps.dynastyprocess.com/calculator"),
               menuSubItem("Database",icon=icon('database'),href="https://apps.dynastyprocess.com/database"),
               #menuSubItem('Crystal Ball',icon=icon('quidditch'),href='https://apps.dynastyprocess.com/crystalball'),
               menuSubItem('Expected Points', icon=icon('chart-line'),href='https://apps.dynastyprocess.com/ep'),
               menuSubItem("GitHub",icon=icon('github'),href="https://apps.dynastyprocess.com/ecr"),
               menuSubItem("More!", icon=icon('rocket'),href="https://dynastyprocess.com/apps")
               )
    )
  ),
  dashboardBody({tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/flatly.css"),
    tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #000;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #555;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                  background-color: #000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                  background-color: #000;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #555;
                                  text-decoration: none;
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #555;
                                  text-decoration: none;
                                }
                                .skin-blue .sidebar-menu > li.active > a{
                                  border-left-color: #fff
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #fff;
                                }
                                .btn {
                                  font-size: 12px;
                                }

                                .selectize-input
                                {font-size:12px;
                                min-height:25px;
                                padding-top:0px;
                                padding-bottom:0px;
                                }

        '))
  )},
    tabItems(
    tabItem(tabName = "mfl",
            fluidRow(
              box(width=12,
              titlePanel("DynastyProcess.com: Crystal Ball App"),
              includeMarkdown("about.md"))),
            fluidRow(
              box(width=4,title="MFL Authentication",
                  textInput('mflusername',NULL,placeholder="Username"),
                  passwordInput('mflpassword',NULL,placeholder="Password"),
                  actionButton('mfllogin',"Load Leagues!")
                  ),
              box(width=8, title="League List",
                  DTOutput('mflleagues'),
                  textOutput('mleaguename')
                  )),
            fluidRow(
              box(title="Season Projections",width=12,
                  DTOutput('mflsummarytbl'),
                  br(),
                  downloadButton('mfldownloadsummary',label="Download")
                  )),
            fluidRow(
              tabBox(width=12,
                     tabPanel('Weekly Schedule',DTOutput('mflweekpivot'),br(),downloadButton('mfldownloadpivot',label='Download')),
                     tabPanel('Details',
                              DTOutput('mfldetails'),
                              br(),
                              downloadButton('mfldownloaddetails',label='Download'))
                     )
            )
    ),
    tabItem(tabName = "sleeper",
            fluidRow(box(
              width = 12,
              titlePanel("DynastyProcess.com: Crystal Ball App"),
              includeMarkdown("about.md")
            )),
            fluidRow(
              box(
                width = 4,
                title = "Sleeper Username",
                textInput(
                  'sleeperusername',
                  NULL,
                  value = "solarpool",
                  placeholder = "Sleeper Username"
                ),
                actionButton('sleeperloaduser', 'Load Leagues!')
              ),
              box(
                width = 8,
                title = "League List",
                DTOutput('sleeperleaguelist'),
                textOutput('sleaguename')
              )
            ),
            fluidRow(
              box(
                title = "Season Projections",
                width = 12,
                DTOutput('sleepersummarytbl'),
                br(),
                downloadButton('sleeperdownloadsummary', label = "Download")
              )
            ),
            fluidRow(tabBox(
              width = 12,
              tabPanel(
                'Weekly Schedule',
                DTOutput('sleeperweekpivot'),
                br(),
                downloadButton('sleeperdownloadpivot', label =
                                 "Download")
              ),
              tabPanel(
                'Details',
                DTOutput('sleeperdetails'),
                br(),
                downloadButton('sleeperdownloaddetails', label =
                                 "Download")
              )
            ))), 
    tabItem(tabName = "espn",
            fluidRow(
              box(width=12,
                  titlePanel("DynastyProcess.com: Crystal Ball App"),
                  includeMarkdown("about.md"))),
            fluidRow(
              box(width=4,title="ESPN League ID",
                  textInput('espnid',NULL,placeholder="LeagueID"),
                  actionButton('espnload',"Load Leagues!")
                  ),
              box(width=8,title="Note",
                  includeMarkdown('espn.md')
              )
              ),
            fluidRow(
              box(title="Season Projections",width=12,
                  DTOutput('espnsummarytbl'),br(),downloadButton('espndownloadsummary')
              )),
            fluidRow(
              tabBox(width=12,
                     tabPanel('Weekly Schedule',DTOutput('espnweekpivot'),br(),downloadButton('espndownloadpivot')),
                     tabPanel('Details',DTOutput('espndetails'),br(),downloadButton('espndownloaddetails'))
              ))
    )
  )
  )
)

server <- function(input, output, session) {
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))
  
  brks<-function(tb,colnum){
    breakvalue<-quantile(range(tb[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
    return(breakvalue)
  }
  
  # MFL code
  
  m_cookie<-eventReactive(input$mfllogin,{GET(paste0("https://api.myfantasyleague.com/2019/login?USERNAME=",input$mflusername,"&PASSWORD=",URLencode(input$mflpassword,reserved=TRUE),"&XML=1")) %>%
    .$cookies %>% .$value %>%   
    URLencode(reserved=TRUE)})
  
  m_leagues<-eventReactive(input$mfllogin,{GET("https://www61.myfantasyleague.com/2019/export?TYPE=myleagues&FRANCHISE_NAMES=1&JSON=1",
                 set_cookies("MFL_USER_ID"=m_cookie()[1],"MFL_PW_SEQ"=m_cookie()[2]),accept_json()) %>% content("text") %>% fromJSON() %>% 
      .$leagues %>% .$league %>% data.frame() %>% 
    mutate(LeagueID=str_sub(url,start=-5),
           Select=shinyInput(actionButton,nrow(.),'button_',label="Select",onclick='Shiny.onInputChange(\"m_select_button\",  this.id)')) %>% 
    select(League=name,Team=franchise_name,LeagueID,Select)})
  
  
  output$mflleagues<-renderDT(m_leagues(),rownames=FALSE,escape=FALSE,selection = 'none',options=list(scrollX=TRUE,pageLength=10))
  
  m <- reactiveValues(leagueid = '',leaguename='')
  
  observeEvent(input$m_select_button, {
    print(input$m_select_button)
    selectedRow <- as.numeric(strsplit(input$m_select_button, "_")[[1]][2])
    m$leagueid <<- as.character(m_leagues()$LeagueID[selectedRow])
    m$leaguename <<- as.character(m_leagues()$League[selectedRow])
  })
  
  output$mleaguename<-renderText(paste("Loaded League:",m$leaguename))
  
  m_franchises<-eventReactive(input$m_select_button,{GET(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=league&L=",m$leagueid,"&APIKEY=&JSON=1"),
                                                         set_cookies("MFL_USER_ID"=m_cookie()[1],"MFL_PW_SEQ"=m_cookie()[2]),accept_json()) %>% 
      content("text") %>% fromJSON() %>% 
    .$league %>% .$franchises %>% .$franchise %>%
      select(ownerid=id,owner=name)})
  
  m_standings<-eventReactive(input$m_select_button,{GET(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=leagueStandings&L=",m$leagueid,"&APIKEY=&JSON=1"),
                                                        set_cookies("MFL_USER_ID"=m_cookie()[1],"MFL_PW_SEQ"=m_cookie()[2]),accept_json()) %>% content("text") %>% fromJSON() %>%
      .$leagueStandings %>% .$franchise %>% 
      select(ownerid=id,pointsfor=pf,potentialpoints=pp,starts_with('h2h'),starts_with('all_play')) %>%
      mutate_at(vars(starts_with('all_play')),as.numeric) %>%
      mutate(record=paste0(h2hw,"-",h2ht,"-",h2hl),
             allplaypct=all_play_w/(all_play_w+all_play_t+all_play_l)) %>%
      select(-starts_with('all_play')) %>%
      inner_join(m_franchises(),by=c('ownerid'='ownerid'))})
  
  
  m_schedule <-eventReactive(input$m_select_button,{GET(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=schedule&L=",m$leagueid,"&APIKEY=&JSON=1"),
                                                        set_cookies("MFL_USER_ID"=m_cookie()[1],"MFL_PW_SEQ"=m_cookie()[2]),accept_json()) %>% content("text") %>% fromJSON() %>%
      .$schedule %>% .$weeklySchedule %>% 
      unnest(matchup) %>% 
      unnest_wider(franchise) %>%
      filter(result=='NA') %>% 
      select(id,week) %>%
      hoist(id,team=1,opp=2)})
  
  m_fullschedule <- eventReactive(input$m_select_button,{m_schedule() %>%
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
  
  m_expectedwins<-eventReactive(input$m_select_button,{m_fullschedule() %>%
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
  
  m_fspivot<-eventReactive(input$m_select_button,{m_fullschedule() %>%
      mutate(weekname=week) %>%
      select(weekname,team_name,win_prob) %>%
      pivot_wider(names_from=weekname,values_from = win_prob,names_prefix = "Week",values_fn = list(win_prob = sum)) %>%
      rename(Team=team_name) %>% 
      mutate(rosWins=rowSums(select(.,starts_with('Week')))) %>% 
      select(Team,rosWins,starts_with('Week')) %>% 
      arrange(desc(rosWins))
    })
  
  m_details<-eventReactive(input$m_select_button,{
    m_fullschedule() %>% 
      select(Week=week,Team=team_name,`Team%`=team_pct,Opp=opp_name,`Opp%`=opp_pct,WinProb=win_prob)
  })
  
  

  
  
  
  
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
  
  output$mflweekpivot<-
    renderDT({
      datatable(m_fspivot(),rownames=FALSE, options=list(scrollX=TRUE,pageLength=25)) %>% 
        formatStyle(-(1:2),backgroundColor = styleInterval(brks(m_fspivot(),-(1:2)),colourlist(20))) %>%
        formatStyle(2,backgroundColor=styleInterval(brks(m_fspivot(),2),colourlist(20))) %>% 
        formatPercentage(-(1:2),1)
    })
  
  output$mfldetails<-
    renderDT({
      m_details() %>% 
        datatable(rownames=FALSE,filter='top',options=list(scrollX=TRUE,pageLength=10)) %>% 
        formatStyle(3,backgroundColor=styleInterval(brks(m_fullschedule(),4),colourlist(20))) %>% 
        formatStyle(5,backgroundColor=styleInterval(brks(m_fullschedule(),6),colourlist(20))) %>%
        formatStyle(6,backgroundColor=styleInterval(brks(m_fullschedule(),7),colourlist(20)))
    })
  
  output$mfldownloadsummary<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballsummary_',m$leaguename,'.csv')},
      content = function(file){write.csv(m_expectedwins(),file,row.names = F)}
      )
  output$mfldownloadpivot<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballweeks_',m$leaguename,'.csv')},
      content = function(file){write.csv(m_fspivot(),file,row.names = F)}
    )
  output$mfldownloaddetails<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballdetails_',m$leaguename,'.csv')},
      content = function(file){write.csv(m_details(),file,row.names = F)}
    )
  
  #Sleeper chunks start here!
  
  sleeperuserid<-eventReactive(input$sleeperloaduser,fromJSON(paste0("https://api.sleeper.app/v1/user/",input$sleeperusername),flatten=TRUE)$user_id)
  
  sleeperleagues<-eventReactive(input$sleeperloaduser,{fromJSON(paste0("https://api.sleeper.app/v1/user/",sleeperuserid(),"/leagues/nfl/",'2019'))%>%
      data.frame() %>% 
      select(Name=name,League_ID=league_id) %>%
      mutate(LeagueSelect=shinyInput(actionButton,nrow(.),'button_',label="Select",onclick='Shiny.onInputChange(\"s_select_button\",  this.id)'))
    })
  
  stringsAsFactors=FALSE
  
  output$sleeperleaguelist<-renderDT(sleeperleagues(),rownames=FALSE,escape=FALSE,selection = 'none',options=list(scrollX=TRUE,pageLength=10))

  s <- reactiveValues(leagueid = '',leaguename='')
  
  observeEvent(input$s_select_button, {
    print(input$s_select_button)
    selectedRow <- as.numeric(strsplit(input$s_select_button, "_")[[1]][2])
    s$leagueid <<- as.character(sleeperleagues()$League_ID[selectedRow])
    s$leaguename <<- as.character(sleeperleagues()$Name[selectedRow])
  })
  
  output$sleaguename<-renderText({paste("Loaded League:",s$leaguename)})

  s_playoffweekstart<-reactive({fromJSON(paste0("https://api.sleeper.app/v1/league/",s$leagueid))$settings$playoff_week_start-1})
  
  s_matchups<-eventReactive(input$s_select_button,{tibble(week=c(1:s_playoffweekstart()))})
  
  s_getmatchups<-function(i,LID){
    fromJSON(paste0("https://api.sleeper.app/v1/league/",LID,"/matchups/",i)) %>%
    select(roster_id,points,matchup_id)
  }
  
  s_matchupdata<-eventReactive(input$s_select_button,{s_matchups()%>%
      mutate(data=lapply(week,s_getmatchups,s$leagueid)) %>% 
      hoist(data,roster_id='roster_id',points='points',matchup_id='matchup_id') %>% 
      unnest(roster_id,points,matchup_id) %>%
      select(-data)})
  
    s_teamlist<-eventReactive(input$s_select_button,{fromJSON(paste0("https://api.sleeper.app/v1/league/",s$leagueid,"/rosters"))%>%
      select(owner_id,roster_id)})
    
    s_users<- eventReactive(input$s_select_button,{fromJSON(paste0("https://api.sleeper.app/v1/league/",s$leagueid,"/users"),flatten=TRUE) %>%
      select(owner=display_name,owner_id=user_id)%>%
      inner_join(s_teamlist(),by="owner_id") %>% 
      mutate_all(as.character)})
    
    s_matchups1<-eventReactive(input$s_select_button,{s_matchupdata() %>% 
      select(opp=roster_id,wk=week,opp_pts=points,m_id=matchup_id) %>%
      mutate(opp=as.character(opp),opp_pts=as.character(opp_pts)) %>%
      unite("merge",opp,opp_pts)})
    
    s_schedule<- eventReactive(input$s_select_button,{s_matchupdata() %>% 
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
    
    s_standings<-eventReactive(input$s_select_button,{s_schedule() %>%
      filter(!is.na(points)) %>%
      group_by(roster_id)%>%
      summarize(wins=sum(wins),allplaywins=sum(allplaywins),weeks=n(),allplaygms=sum(gms)) %>%
      ungroup() %>% 
      mutate(losses=weeks-wins,allplaypct=round(allplaywins/allplaygms,digits=3),allplaylosses=allplaygms-allplaywins,roster_id=as.character(roster_id))})
    
    s_fullschedule<- eventReactive(input$s_select_button,{s_schedule() %>%
      filter(is.na(points)) %>% 
      select(roster_id,week,opp)%>%
      mutate(roster_id=as.character(roster_id)) %>% 
      nest_join(s_standings(),by='roster_id',name='teaminfo') %>%
      nest_join(s_standings(),by=c('opp'='roster_id'),name='oppinfo')%>%
      hoist(teaminfo,team_pct='allplaypct')%>%
      hoist(oppinfo,opp_pct='allplaypct')%>%
      mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3)) %>%
      select(roster_id,week,opp,team_pct,opp_pct,win_prob)})
    
    s_summary<-eventReactive(input$s_select_button,{s_fullschedule() %>% 
      group_by(roster_id) %>% 
      summarize(rosWins=sum(win_prob),rosGms=n()) %>%
      ungroup() %>% 
      left_join(s_standings(),by='roster_id') %>% 
      left_join(s_users(),by='roster_id') %>%
      mutate(rosLosses=rosGms-rosWins) %>% 
      select(Team=owner,`AllPlay%`=allplaypct,Wins=wins,Losses=losses,rosWins,rosLosses) %>% 
      mutate(TotalWins=Wins+rosWins,TotalLosses=Losses+rosLosses) %>% 
      arrange(desc(TotalWins))})
    
    s_fspivot<-eventReactive(input$s_select_button,{s_fullschedule() %>% 
      select(roster_id,week,win_prob) %>% 
      nest_join(s_users(),by='roster_id',name='users') %>% 
      hoist(users,Team='owner') %>% 
      select(-roster_id,-users) %>% 
      pivot_wider(names_from = week, values_from=win_prob, names_prefix = "Week", values_fn = list(win_prob = sum)) %>% 
      mutate(rosWins=rowSums(select(.,starts_with('Week')))) %>% 
      select(Team,rosWins,starts_with('Week')) %>% 
      arrange(desc(rosWins))
      })
    
    s_details<-eventReactive(input$s_select_button,{
      s_fullschedule() %>%
        nest_join(s_users(),by='roster_id',name='users') %>%
        nest_join(s_users(),by=c('opp'='roster_id'),name='oppinfo') %>% 
        hoist(users,Team='owner') %>% 
        hoist(oppinfo,Opp='owner') %>% 
        select(-roster_id,-opp,-users,-oppinfo) %>% 
        select(Week=week,Team,`Team%`=team_pct,Opp,`Opp%`=opp_pct,WinProb=win_prob)
    })

  
  output$sleepersummarytbl<-renderDT({
    req(input$s_select_button)
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
  
  output$sleeperweekpivot<-
    renderDT({
      datatable(s_fspivot(),rownames=FALSE, options=list(scrollX=TRUE,pageLength=25)) %>% 
        formatStyle(-(1:2),backgroundColor = styleInterval(brks(s_fspivot(),-(1:2)),colourlist(20))) %>%
        formatStyle(2,backgroundColor = styleInterval(brks(s_fspivot(),2),colourlist(20))) %>% 
        formatPercentage(-(1:2),1)
    })
  
  output$sleeperdetails<-
    renderDT({
      s_details() %>% 
        datatable(rownames=FALSE,filter='top',options=list(scrollX=TRUE,pageLength=10)) %>% 
        formatStyle(3,backgroundColor=styleInterval(brks(s_fullschedule(),4),colourlist(20))) %>% 
        formatStyle(5,backgroundColor=styleInterval(brks(s_fullschedule(),5),colourlist(20))) %>%
        formatStyle(6,backgroundColor=styleInterval(brks(s_fullschedule(),6),colourlist(20)))
    })
  
  output$sleeperdownloadsummary<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballsummary_',s$leaguename,'.csv')},
      content = function(file){write.csv(s_summary(),file,row.names = F)}
    )
  output$sleeperdownloadpivot<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballweeks_',s$leaguename,'.csv')},
      content = function(file){write.csv(s_fspivot(),file,row.names = F)}
    )
  output$sleeperdownloaddetails<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballdetails_',s$leaguename,'.csv')},
      content = function(file){write.csv(s_details(),file,row.names = F)}
    )
  
  
  
  
  # ESPN chunks
  
  espn<-eventReactive(input$espnload,{fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                         input$espnid,
                         '?view=mMatchupScore',
                         '&view=mTeam', 
                         '&view=mNav'),flatten=TRUE)})
  
  e_owners <- eventReactive(input$espnload, {
    espn()$members %>%
      select(id, displayName) %>%
      nest_join(espn()$teams,
                by = c('id' = 'primaryOwner'),
                name = 'teams') %>%
      hoist(teams, team_id = 'id') %>%
      select(-teams) %>% 
      mutate(team_id=as.character(team_id))
  })
  
  output$eleaguename<-renderText(paste('Loaded League:',espn()$settings$name))
  
  e_schedule1<-eventReactive(input$espnload,{espn()$schedule %>% 
    select(winner, contains('id'),ends_with('totalPoints')) %>% 
    rename(gameid=id,week=matchupPeriodId,
           team_id=away.teamId, opp_id=home.teamId,
           team_points=away.totalPoints, opp_points=home.totalPoints)})
  
  e_schedule<-eventReactive(input$espnload,{espn()$schedule %>% 
    select(winner, contains('id'),ends_with('totalPoints')) %>% 
    rename(gameid=id,week=matchupPeriodId,
           team_id=home.teamId, opp_id=away.teamId,
           team_points=home.totalPoints, opp_points=away.totalPoints) %>% 
    bind_rows(e_schedule1()) %>% 
    arrange(week,team_id)})
  
  e_standings<-eventReactive(input$espnload,{e_schedule() %>% 
    filter(winner!='UNDECIDED') %>% 
    group_by(week) %>% 
    mutate(wins=if_else(team_points>opp_points,1,0),
           APgms=n()-1,
           APwins=rank(team_points)-1) %>% 
    ungroup() %>% 
    group_by(team_id) %>% 
    summarize(wins=sum(wins),allplaywins=sum(APwins),weeks=n(),allplaygms=sum(APgms)) %>% 
    mutate(losses=weeks-wins,
           allplaypct=round(allplaywins/allplaygms,digits=3),
           allplaylosses=allplaygms-allplaywins)})
  
  e_fullschedule<- eventReactive(input$espnload,{e_schedule() %>%
    filter(winner=='UNDECIDED') %>% 
    select(team_id,week,opp_id)%>%
    nest_join(e_standings(),by='team_id',name='teaminfo') %>%
    nest_join(e_standings(),by=c('opp_id'='team_id'),name='oppinfo')%>%
    hoist(teaminfo,team_pct='allplaypct')%>%
    hoist(oppinfo,opp_pct='allplaypct')%>%
    mutate(win_prob=round(team_pct/(team_pct+opp_pct),digits=3)) %>%
    select(team_id,week,opp_id,team_pct,opp_pct,win_prob)})
  
  e_summary<-eventReactive(input$espnload,{e_fullschedule() %>% 
    group_by(team_id) %>% 
    summarize(rosWins=sum(win_prob),rosGms=n()) %>%
    ungroup() %>% 
    left_join(e_standings(),by='team_id') %>%
    mutate(team_id=as.character(team_id)) %>% 
    left_join(e_owners(),by=c('team_id'='team_id')) %>%
    mutate(rosLosses=rosGms-rosWins) %>% 
    select(Team=displayName,`AllPlay%`=allplaypct,Wins=wins,Losses=losses,rosWins,rosLosses) %>% 
    mutate(TotalWins=Wins+rosWins,TotalLosses=Losses+rosLosses) %>% 
    arrange(desc(TotalWins))})
  
  e_fspivot <- eventReactive(input$espnload, {
    e_fullschedule() %>%
      select(team_id, week, win_prob) %>%
      mutate(team_id = as.character(team_id)) %>%
      nest_join(e_owners(),
                by = c('team_id' = 'team_id'),
                name = 'owners') %>%
      hoist(owners, Team = 'displayName') %>%
      select(-team_id,-owners) %>%
      pivot_wider(
        names_from = week,
        values_from = win_prob,
        names_prefix = "Week", 
        values_fn = list(win_prob = sum)) %>%
      mutate(rosWins = rowSums(select(., starts_with('Week')))) %>%
      select(Team, rosWins, starts_with('Week')) %>% 
      arrange(desc(rosWins))
  })
  
  e_details<-eventReactive(input$espnload,{
    e_fullschedule() %>%
      mutate(team_id = as.character(team_id),opp_id=as.character(opp_id)) %>%
      nest_join(e_owners(),by='team_id',name='users') %>%
      nest_join(e_owners(),by=c('opp_id'='team_id'),name='oppinfo') %>% 
      hoist(users,Team='displayName') %>% 
      hoist(oppinfo,Opp='displayName') %>% 
      select(-team_id,-opp_id,-users,-oppinfo) %>% 
      select(Week=week,Team,`Team%`=team_pct,Opp,`Opp%`=opp_pct,WinProb=win_prob)
  })
  
  output$espnsummarytbl<-renderDT({
    req(input$espnload)
    e_sumtbl<-datatable(e_summary(), rownames=FALSE, options=list(scrollX=TRUE,pageLength=25))
    for(colnum in c(2,3,5,7)){
      e_sumtbl<-e_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(e_summary(),colnum),colourlist(20)))%>%
        formatPercentage(2,1)
    }
    for(colnum in c(4,6,8)){
      e_sumtbl<-e_sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(e_summary(),colnum),rev(colourlist(20))))
    }
    e_sumtbl
  })
  
  output$espnweekpivot<-
    renderDT({
      datatable(e_fspivot(),rownames=FALSE, options=list(scrollX=TRUE,pageLength=25)) %>% 
        formatStyle(-(1:2),backgroundColor = styleInterval(brks(e_fspivot(),-(1:2)),colourlist(20))) %>%
        formatStyle(2,backgroundColor = styleInterval(brks(e_fspivot(),2),colourlist(20))) %>% 
        formatPercentage(-(1:2),1)
    })
  
  output$espndetails<-
    renderDT({
      e_details() %>% 
        datatable(rownames=FALSE,filter='top',options=list(scrollX=TRUE,pageLength=10)) %>% 
        formatStyle(3,backgroundColor=styleInterval(brks(e_fullschedule(),4),colourlist(20))) %>% 
        formatStyle(5,backgroundColor=styleInterval(brks(e_fullschedule(),5),colourlist(20))) %>%
        formatStyle(6,backgroundColor=styleInterval(brks(e_fullschedule(),6),colourlist(20)))
    })
  
  output$espndownloadsummary<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballsummary_',espn()$settings$name,'.csv')},
      content = function(file){write.csv(e_summary(),file,row.names = F)}
    )
  output$espndownloadpivot<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballweeks_',espn()$settings$name,'.csv')},
      content = function(file){write.csv(e_fspivot(),file,row.names = F)}
    )
  output$espndownloaddetails<-
    downloadHandler(
      filename = function(){paste0('DP_crystalballdetails_',espn()$settings$name,'.csv')},
      content = function(file){write.csv(e_details(),file,row.names = F)}
    )
  
} #end of server segment

shinyApp(ui, server)