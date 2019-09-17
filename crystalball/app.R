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
    tabPanel("Projections",
             br(),
             fluidRow(column(12,DTOutput('summarytbl')))),
    tabPanel("Schedule",
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
    inner_join(franchises())})
    
  
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
    nest_join(standings(),by=c('team'='ownerid'),name='standings') %>%
    hoist(standings,h2hw="h2hw",h2ht="h2ht",h2hl="h2hl") %>%
    mutate_at(vars("h2hw","h2ht","h2hl"),as.numeric) %>%
    mutate(total_wins=ewins+h2hw, total_losses=elosses+h2hl) %>%
    select(team,team_name,wins=h2hw,losses=h2hl,ewins,elosses,total_wins,total_losses) %>%
    arrange(desc(total_wins))})
  

  
  colors<-colorRampPalette(brewer.pal(3,'PuOr'))
  
  brks_dpos<-function(colnum){
    breakvalue<-quantile(range(pivot_dpos[colnum]),probs=seq(0.01,0.99,0.01),na.rm=TRUE)
    return(breakvalue)
  }
  
  output$summarytbl<- renderDT(expectedwins(), options=list(
    pageLength=25
  ))
  
  output$detailstbl<- renderDT(fullschedule(), options=list(
    pageLength=50
  ))
  

}

shinyApp(ui, server)