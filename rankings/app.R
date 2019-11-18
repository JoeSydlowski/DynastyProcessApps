library(tidyverse)
library(shiny)
library(DT)
library(yaml)


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

  output$rb<-renderDT(server=F,{
    datatable(
      rb(),
      colnames = c(`Your Rank` = 1),
      rownames = TRUE,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(0, 'asc'))),
      callback = JS("table.on('row-reorder',function(e, details, changes){Shiny.onInputChange('table_row_reorder', details);});")
      )
  })
  
  observe(print(input$table_row_reorder))
  
  

    
}

shinyApp(ui, server)