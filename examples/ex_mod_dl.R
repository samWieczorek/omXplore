library(shiny)

ui <- fluidPage(
  dl_ui("dl")
)

server <- function(input, output, session) {
  data(Exp1_R25_prot, package='DaparToolshedData', envir = environment())
  
  dl_server("dl",
            dataIn = reactive({Exp1_R25_prot}),
            extension = c('csv', 'xlsx', 'RData')
  )
}

if(interactive())
  shinyApp(ui = ui, server = server)