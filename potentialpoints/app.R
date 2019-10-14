library(jsonlite)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
# library(here)

# setwd(here())

ui <-
  dashboardPage(skin = "blue", title = "DynastyProcess Apps: Potential Points Calculator",
    dashboardHeader(title = a(href = "https://dynastyprocess.com", img(src = "logo-horizontal.png", width ='100%')), titleWidth = 250),
    dashboardSidebar(width = 250,
      sidebarMenu(menuItem("ESPN Potential Points", tabName = "espnpp", icon = icon("tv"))
      ),
      sidebarMenu(
        menuItem(
          "More from DynastyProcess:",
          icon = icon("rocket"),
          menuSubItem("Calculator", icon =
                        icon('calculator'), href = "https://apps.dynastyprocess.com/calculator"),
          menuSubItem("Database", icon =
                        icon('database'), href = "https://apps.dynastyprocess.com/database"),
          menuSubItem("Crystal Ball", icon =
                        icon('quidditch'), href = "https://apps.dynastyprocess.com/crystalball"),
          menuSubItem("More!", icon =
                        icon('rocket'), href = "https://dynastyprocess.com/apps")
        ))
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
      tabItem(tabName='espnpp',
              fluidRow(
                box(width=12, 
                    titlePanel("DynastyProcess: Potential Points Calculator"),
                    includeMarkdown('about.md')
                    )
              ),
              fluidRow(
                box(width=8,
                    textInput('leagueid',label='ESPN League ID',value='1178049',placeholder="(Public Leagues Only!)"),
                    sliderInput('weekselect',label = "Select Weeks", value=c(1,17),min=1,max=17, ticks=FALSE),
                    actionButton('load','Calculate!',icon=icon('calculator')),
                    br(),
                    textOutput('leaguename')
                    ),
                box(width=4,title='Downloads',
                    downloadButton('downloadseason',label="Download PP Summary"),
                    downloadButton('downloadweek',label='Download Weekly Breakdown'))
              ),
              fluidRow(
                    tabBox(width = 12,title = "Summary",side='right',
                    tabPanel('Total',DTOutput('summary_season')),
                    tabPanel('Weekly Breakdown',DTOutput('summary_week'))
              )
),

              fluidRow(
                box(title='Details',width=12,solidHeader = TRUE,
                    DTOutput('details'))
              )

              )
      )
      
    )
  )
  

server <- function(input, output, session) {
  
  espnbasic<- eventReactive(input$load,fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                              input$leagueid,
                              '?view=mSettings',
                              '&view=mTeam'),flatten=TRUE))
  
  leaguename<-reactive(espnbasic()$settings$name)
  
  currentweek<-reactive(espnbasic()$status$currentMatchupPeriod)
  
  maxweek<-reactive(if_else(input$weekselect[2]<=currentweek(),input$weekselect[2],currentweek()))
  
  output$leaguename<-renderText(paste('Loaded:',leaguename()))
  
  teams<-reactive({espnbasic()$teams %>% 
    select(id,primaryOwner, location, nickname) %>% 
    mutate(team_name=paste(location,nickname)) %>% 
    select(id,primaryOwner,team_name)})
  
  owners <- reactive({
    espnbasic()$members %>%
      select(id, owner_name = displayName) %>%
      nest_join(teams(),
                by = c('id' = 'primaryOwner'),
                name = 'teams') %>%
      hoist(teams, team_id = 'id', team_name = 'team_name') %>%
      select(-teams) %>%
      unnest(team_id, team_name)
  })

  ppfunction<-function(league_id,scoreweek){
    
    optimal_lineups<-tibble()
    
    espn<- fromJSON(paste0('https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/',
                           league_id,
                           '?scoringPeriodId=',
                           scoreweek,
                           '&view=mMatchupScore',
                           '&view=mBoxscore',
                           #'&view=mScoreboard',
                           #'&view=mTeam', 
                           #'&view=mRoster',
                           '&view=mSettings',
                           '&view=mRosterSettings'
                           #'&view=kona_player_info',
                           #'&view=mNav'
                           ),flatten=TRUE)

    lineup_settings<-tibble(lineup_id=c(0,2,3,4,5,6,7,16,17,20,21,23,
                                        8,9,10,11,24,12,13,14,15),
                            pos=c('QB','RB','RB/WR','WR','WR/TE','TE',
                                  'OP','DST','K','BE','IR','FLEX',
                                  'DT','DE','LB','DL','EDR','CB','S','DB','DP'),
                            priority=c(1,2,5,3,6,4,8,9,10,0,0,11,
                                       12,13,16,14,15,17,18,19,20)) %>%
      arrange(lineup_id) %>% 
      mutate(lineup_id=as.character(lineup_id)) %>% 
      left_join(tibble(lineup_id=as.character(names(espn$settings$rosterSettings$lineupSlotCounts)),count=espn$settings$rosterSettings$lineupSlotCounts),
                by='lineup_id') %>% 
      filter(count!=0 & priority!=0) %>% 
      arrange(priority)
    
    
    schedule<-espn$schedule %>%
      select(week=matchupPeriodId,away.teamId,away.entries=away.rosterForCurrentScoringPeriod.entries,
             home.teamId,home.entries=home.rosterForCurrentScoringPeriod.entries, 
             home.points=home.totalPoints,away.points=away.totalPoints) %>% 
      filter(away.entries!='NULL')
    
    playerweeks <- schedule %>%
      select(
        week,
        home.teamId = away.teamId,
        home.entries = away.entries,
        home.points = away.points
      ) %>%
      bind_rows(schedule) %>%
      select(week,
             team_id = home.teamId,
             score = home.points,
             entries = home.entries) %>%
      hoist(
        entries,
        actual_lineup = 'lineupSlotId',
        player_id = 'playerId',
        points = 'playerPoolEntry.appliedStatTotal',
        player = 'playerPoolEntry.player.fullName',
        eligible = 'playerPoolEntry.player.eligibleSlots'
      ) %>%
      unnest(actual_lineup, player_id, points, player, eligible) %>%
      unnest_longer(eligible) %>% 
      mutate(eligible=as.character(eligible)) %>% 
      select(-entries)
    
    unusedplayers<-playerweeks
    starters<-tibble()
    
    for (i in lineup_settings$priority) {
      pos<-lineup_settings %>% 
        filter(priority==i) %>% 
        nest_join(unusedplayers,by=c('lineup_id'='eligible')) %>% 
        unnest_wider(unusedplayers) %>% 
        unnest(-(1:3)) %>% 
        group_by(team_id) %>% 
        mutate(rank=rank(desc(points),ties.method=c('first'))) %>% 
        filter(rank<=count)
      
      starters<-bind_rows(starters,pos) %>% 
        arrange(team_id,priority,desc(points))
      
      unusedplayers<-unusedplayers %>% 
        anti_join(pos,by=c('player_id'))
    }
    
    optimal_lineups<-bind_rows(optimal_lineups,starters) %>%
      nest(data=everything())
    
    return(optimal_lineups)
  }
  
  details<-eventReactive(input$load,{
    tibble(league_id=input$leagueid,weeklist=c(input$weekselect[1]:maxweek())) %>%
      rowwise() %>% 
      mutate(lineups=lapply(league_id,ppfunction,weeklist)) %>% 
      unnest_wider(lineups) %>% 
      unnest_wider(data) %>% 
      unnest_wider(3) %>% 
      unnest(-(1:2)) %>%
      left_join(owners(),by="team_id") %>% 
      select(Week=week,Owner=owner_name,Team=team_name,Pos=pos,ActualScore=score,Player=player,Points=points)
  })
  
  summary_week<-eventReactive(input$load,{
    details() %>% 
      group_by(Owner,Week,Team) %>%
      summarize(ActualScore=mean(ActualScore),PotentialScore=sum(Points)) %>% 
      ungroup()
  })
  
  summary_season<-eventReactive(input$load,{
    summary_week() %>% 
      group_by(Owner,Team) %>% 
      summarize(ActualScore=sum(ActualScore),PotentialScore=sum(PotentialScore)) %>% 
      arrange(desc(PotentialScore))
  })
  
  
  output$details<-renderDT(details(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=25))
  
  output$summary_week<-renderDT(summary_week(),rownames=FALSE,options=list(scrollX=TRUE,lengthChange=FALSE,pageLength=50))
  
  output$summary_season<-renderDT(summary_season(),rownames=FALSE,options=list(scrollX=TRUE,lengthChange=FALSE,pageLength=50))
  
  output$downloadseason<-downloadHandler(
    filename=function(){paste0('Potential Points:',leaguename(),'.csv')},
    content=function(file){write.csv(summary_season(),file,row.names=FALSE)}
  )
  output$downloadweek<-downloadHandler(
    filename=function(){paste0('Potential Points:',leaguename(),'.csv')},
    content=function(file){write.csv(summary_week(),file,row.names=FALSE)}
  )
  
}

shinyApp(ui, server)