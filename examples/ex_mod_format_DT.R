

library(shiny)
library(shinyjs)

ui <- mod_format_DT_ui("dt")

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    selected = NULL
  )
  
  hcStyle = list(
    cols = colnames(iris)[1:3],
    vals = colnames(iris)[4],
    unique = xxxx,
    pal = xxx
  )

  rv$selected <- mod_format_DT_server("dt", data = reactive({iris}) )
  
   observeEvent(rv$selected(), {
     print(paste0('Selected line(s):', rv$selected()))
   })
}

shinyApp(ui = ui, server = server)
