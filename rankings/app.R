library(tidyverse)
library(shiny)
library(DT)

ui <- fluidPage(
  fluidRow(column(4,
                  DTOutput('rb')
                  ))
  
)

server <- function(input, output, session) {

  fantasypros<-read.csv("ecr_20191112.csv") %>% 
    select(Player,Team,Pos,starts_with('do'),starts_with('dp')) %>% 
    filter(Pos %in% c('QB','RB','WR','TE'))
  
  rb<-fantasypros %>% 
    filter(Pos == 'RB') %>% 
    arrange(dpECR) %>%
    filter(!is.na(dpECR)) %>% 
    # mutate_all(as.character) %>% 
    # rowid_to_column(var='YourRank') %>%
    select(Player,Team,Pos,dpECR) %>% 
    reactiveVal()

  output$rb<-renderDT(server=FALSE,{
    datatable(rb(),
              colnames = c(`Your Rank`=1,Player=2,Team=3,Pos=4,dpECR=5),
              rownames=TRUE,
              extensions = 'RowReorder',
              selection='none',
              options=list(
                rowReorder=list(selector='tr'),
                order=list(c(0,'asc'))
              ))
  })
    
}

shinyApp(ui, server)