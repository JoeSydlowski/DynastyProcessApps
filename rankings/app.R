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
    rowid_to_column(var='Your Rank') %>%
    mutate(`Your Rank`=as.numeric(`Your Rank`),
           z = (`Your Rank`-dpECR)/dpSD) %>% 
    select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z) %>% 
    reactiveVal()
  
  num_rb<-fantasypros %>% 
    filter(Pos == 'RB',!is.na(dpECR)) %>% 
    nrow()
  
  output$rb<-renderDT(server=F,{
    
    datatable(
      rb(),
      rownames = F,
      extensions = 'RowReorder',
      selection = 'none',
      options = list(rowReorder = list(selector = 'tr'),
                     order = list(c(3, 'asc'))),
      callback = JS("table.on('row-reorder',function(e, details, all){Shiny.onInputChange('table_row_reorder', JSON.stringify(details));});")
      ) %>% 
      formatRound('z',1)
  })
  
  table_order<-reactiveVal(seq(1,num_rb))
  
  observeEvent(input$table_row_reorder,{
    
    info<-input$table_row_reorder
    
    if (is.null(info)|class(info) !='character'){return()}
    
    info<-read_yaml(text=info)
    s<<-tibble(info)

    if(length(info)==0){return()}
    
    .order<-seq(1,num_rb)
    .new_order<-.order
    
    for (i in 1:length(info)){
      j<- info[[i]]
      .new_order[(j$oldPosition+1)]<-.order[(j$newPosition+1)]
    }
    
    table_order(.new_order)
    
    df<-rb() %>%
      select(-`Your Rank`) %>% 
      mutate(`Your Rank`=table_order(),
             z = (`Your Rank`-dpECR)/dpSD) %>% 
      arrange(`Your Rank`) %>% 
      select(Player,Team,Pos,`Your Rank`,dpECR,dpSD,z)
    
    rb(df)
    
  })
  
  
  

    
}

shinyApp(ui, server)