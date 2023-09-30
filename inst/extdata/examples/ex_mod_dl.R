library(shiny)

ui <- fluidPage(
  dl_ui("dl")
)

server <- function(input, output, session) {
  data(ft, envir = environment())
  
  dl_server("dl",
            dataIn = reactive({ft}),
            extension = c('csv', 'xlsx', 'RData')
  )
}

if(interactive())
  shinyApp(ui = ui, server = server)