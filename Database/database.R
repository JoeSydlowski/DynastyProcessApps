#library(rsconnect)
#rsconnect::setAccountInfo(name='dynastyprocess', token='A4F52D8E95D794DAD045298C4FA2FCFD', secret='ilBqKgDSeQy9qLy72vc7TOwNz5wHPExdpjeaocPt')
#setwd("~/Fantasy Football/Excel Sheets/DynastyProcess")

library(shiny)
library(curl)
library(DT)
library(shinythemes)

x <- read.csv(curl("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/database.csv"))
cols = c(24:32,46:51)

ui <- fluidPage(
   #shinythemes::themeSelector(),
   theme = shinytheme("spacelab"),
   # h1(id = "big-heading",
   #    "DynastyProcess.com Database",
   #    align = "center"),
   # tags$style(HTML("#big-heading{color: white;
   #                 background-color:blue;
   #                 height: 100px;
   #                 font-size:60px;}")),
   

   column(10, offset = 4, titlePanel("DynastyProcess.com Database")),
   selectizeInput("select",
                  "Select columns:", 
                  choices = list(
                          PlayerVariables = names(x)[12:22],
                          Fantasypros = names(x)[56:62],
                          Contract = names(x)[54:55],
                          Snaps = names(x)[52:53],
                          Stats2018 = names(x)[cols],
                          Vitals = names(x)[33:45],
                          PlayerIDs = names(x)[1:11]),
                  selected = c("mergename", "pos", "team", "age", "draft_year", "draft_round",
                               "offSnaps.", "offSnaps", "dynoECR", "dynpECR", "redpECR"),
                  multiple = TRUE,
                  width = "50%"),
   hr(),
   actionButton("clear1", "Reset All Filters"),
   downloadButton("downloadData", "Download"),
   hr(),
   DTOutput("results"),
   hr(),
   p(HTML(paste0("The data for this page is provided by ",
     a(href = "https://dynastyprocess.com/downloads/database/", "https://dynastyprocess.com/downloads/database/"),
     " and aggregated from FantasyPros ECR,
      AirYards.com, Spotrac, Pro-Football-Reference, RAS Athletic Data (@MathBomb), and MFL/Sleeper APIs.")))
)

server <- function(input, output, session) {
  df <- reactive(
    data.frame(x[,input$select, drop = FALSE])
  )
  
  output$results <- renderDT({
    datatable( df(),
               filter = 'top',
               options = list(pageLength = 50,
                              scrollX =TRUE,
                              columnDefs = list(list(className = 'dt-head-left', targets = "_all"))),
               class = 'compact stripe') %>%
      formatStyle(columns = names(dplyr::select_if(df(), is.numeric)),
                  'text-align' = 'left')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {"DynastyProcessDatabase.csv"},
    content = function(file) {write.csv(df(), file)}
  )
  
  proxy = dataTableProxy('results')
  
  observeEvent(input$clear1, {proxy %>% clearSearch()}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

