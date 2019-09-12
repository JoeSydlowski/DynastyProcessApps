library(shiny)


shinyUI(fluidPage(
    
        mainPanel(
            DTOutput("pivot"),
            DTOutput("sched")
        )
    )
)

