library(jsonlite)
library(rvest)
library(tidyr)
library(dplyr)
library(DT)
library(RColorBrewer)
#library(lubridate)
library(shiny)

ui <- fluidPage(
  theme = "css/flatly.css",
  withTags(
    nav(class="navbar navbar-default navbar-static-top", role="navigation",
        div(class="container-fluid",
            div(class="navbar-header",
                span(class="navbar-brand", style="padding: 5px 0px 5px 5px",
                     a(href="https://dynastyprocess.com",img(src="logo-horizontal.png",width="auto"))
                ),
                button(type="button", class="navbar-toggle", `data-toggle`="collapse", `data-target`="#myNavbar", style="padding-left:10px",
                       span(class="icon-bar"),
                       span(class="icon-bar"),
                       span(class="icon-bar")
                )
            ),
            div(class="collapse navbar-collapse", id="myNavbar",
                ul(class="nav navbar-nav",
                   li(a(href="http://apps.dynastyprocess.com/database",strong("Database"))
                   ),
                   li(class="dropdown active",
                      a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="Calculator",`aria-expanded`="false", href="https://apps.dynastyprocess.com/calculator", strong("Calculator"),b(class="caret")),
                      ul(class="dropdown-menu",
                         li(class="active",a(href="#",strong("Normal Mode"))),
                         li(a(href="https://apps.dynastyprocess.com/evil-calc",strong("Dark Mode")))
                      )
                   ),
                   li(
                     a(href="http://apps.dynastyprocess.com/rookie-adp",strong("Rookie ADP"))
                   ),
                   li(class="dropdown",
                      a(class="dropdown-toggle",`data-toggle`="dropdown", `data-value`="More Awesome Apps",`aria-expanded`="false", href="#", strong("More Awesome Apps"),b(class="caret")),
                      ul(class="dropdown-menu",
                         li(a(href="http://apps.dynastyprocess.com/arbitrage",strong("Arbitrage"))),
                         li(a(href="http://apps.dynastyprocess.com/ecr",strong("ECR Explorer"))),
                         li(a(href="http://apps.dynastyprocess.com/cohort",strong("Cohort")))
                      )
                   )
                ))
        )
    )
  ),
  titlePanel("DynastyProcess.com: MFL Crystal Ball"),
  div(
    fluidRow(
      column(12, includeMarkdown("about.md"))
    )),
  hr(),
  textInput('franchiseid',"MFL League ID",value="54040",placeholder="Should be five digits!"),
  actionButton('loaddata','Load!'),
  br(),
  br(),
  tabsetPanel(
    tabPanel("Season Projections",icon=icon("chart-bar"),
             br(),
             fluidRow(column(12,DTOutput('summarytbl')))),
    tabPanel("Schedule",icon=icon("calendar"),
             br(),
             fluidRow(column(12,DTOutput('detailstbl'))))
  )
)

server <- function(input, output, session) {
  
  reactive(req(input$franchiseid))
  
  franchises<-eventReactive(input$loaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=league&L=",input$franchiseid,"&APIKEY=&JSON=1"))$league$franchises$franchise %>%
    select(ownerid=id,owner=name)})
  
  standings<-eventReactive(input$loaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=leagueStandings&L=",input$franchiseid,"&APIKEY=&JSON=1"))$leagueStandings$franchise %>% 
    select(ownerid=id,pointsfor=pf,potentialpoints=pp,starts_with('h2h'),starts_with('all_play')) %>%
    mutate_at(vars(starts_with('all_play')),as.numeric) %>%
    mutate(record=paste0(h2hw,"-",h2ht,"-",h2hl),
           allplaypct=all_play_w/(all_play_w+all_play_t+all_play_l)) %>%
    select(-starts_with('all_play')) %>%
    inner_join(franchises(),by=c('ownerid'='ownerid'))})
    
  
  schedule <-eventReactive(input$loaddata,{fromJSON(paste0("https://www03.myfantasyleague.com/2019/export?TYPE=schedule&L=",input$franchiseid,"&APIKEY=&JSON=1"))$schedule$weeklySchedule %>% 
    unnest(matchup) %>% 
    unnest_wider(franchise) %>%
    filter(result=='NA') %>% 
    select(id,week) %>%
    hoist(id,team=1,opp=2)})
  
  fullschedule <- eventReactive(input$loaddata,{schedule() %>%
    select(week,team=opp,opp=team) %>%
    bind_rows(schedule()) %>%
    mutate(week=as.numeric(week)) %>% 
    arrange(week,team) %>%
    nest_join(standings(), by=c("team"="ownerid"),name='teaminfo') %>%
    nest_join(standings(),by=c("opp"="ownerid"),name='oppinfo') %>%
    hoist(teaminfo,team_name="owner",team_pct="allplaypct") %>%
    hoist(oppinfo,opp_name="owner",opp_pct="allplaypct") %>%
    select(-teaminfo,-oppinfo,-opp) %>%
    mutate(
      team_pct = round(team_pct,digits=3),
      opp_pct = round(opp_pct, digits=3),
      win_prob=round(team_pct/(team_pct+opp_pct),digits=3))})
  
  expectedwins<-eventReactive(input$loaddata,{fullschedule() %>%
      group_by(team,team_name) %>%
      summarize(ewins=sum(win_prob,na.rm=TRUE),
                elosses=n()-ewins) %>%
      ungroup() %>%
      nest_join(standings(),by=c('team'='ownerid'),name="standings") %>%
      hoist(standings,h2hw="h2hw",h2ht="h2ht",h2hl="h2hl") %>%
      mutate_at(vars("h2hw","h2ht","h2hl"),as.numeric) %>%
      mutate(TotalWins=ewins+h2hw, TotalLosses=elosses+h2hl) %>%
      select(Team = team_name,Wins=h2hw,Losses=h2hl,eWins=ewins,eLosses=elosses,TotalWins,TotalLosses) %>%
      arrange(desc(TotalWins))})
  
  fspivot<-eventReactive(input$loaddata,{fullschedule() %>%
    mutate(weekname=paste0("Week",week)) %>%
    select(weekname,team_name,win_prob) %>%
    pivot_wider(names_from=weekname,values_from = win_prob) %>%
    rename(Team=team_name)})
  
  
  colourlist<-colorRampPalette(brewer.pal(3,'PRGn'))

  
  brks<-function(tb,colnum){
    breakvalue<-quantile(range(tb[colnum]),probs=seq(0.05,0.95,0.05),na.rm=TRUE)
    return(breakvalue)
  }
  
  
  output$summarytbl<-renderDT({
    sumtbl<-datatable(expectedwins(), options=list(pageLength=25,rownames=FALSE))
    for(colnum in c(2,4,6)){
      sumtbl<-sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(expectedwins(),colnum),colourlist(20)))
    }
    for(colnum in c(3,5,7)){
      sumtbl<-sumtbl%>%formatStyle(colnum,backgroundColor = styleInterval(brks(expectedwins(),colnum),rev(colourlist(20))))
    }
    sumtbl
  })

  output$detailstbl<-
    renderDataTable({
      datatable(fspivot(), options=list(
        pageLength=50)) %>% 
        formatStyle(-1,backgroundColor = styleInterval(brks(fspivot(),-1),colourlist(20)))})
  

}

shinyApp(ui, server)