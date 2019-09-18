library(jsonlite)
library(tidyr)
library(dplyr)
library(DT)
library(RColorBrewer)
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
              box(width=12,
                  textInput('mflid',"MFL League ID",value="54040",placeholder="Should be five digits!"),
                  actionButton('mflloaddata','Load!')))),
            fluidRow(
              box(title="Season Projections",width=12,
                  DTOutput('mflsummarytbl')
                  )),
            fluidRow(box(width=12,title="Schedule",DTOutput('mfldetailstbl')))
    ),
    tabItem(tabName = "sleeper",
            fluidRow(
              box(title="bleh")
            )
    )
  )
  )
)

server <- function(input, output, session) {
  # MFL code
  
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
  
}

shinyApp(ui, server)