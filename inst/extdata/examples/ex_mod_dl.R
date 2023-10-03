library(DaparViz)

ui <- fluidPage(
  dl_ui("dl")
)

server <- function(input, output, session) {
  data(vData_ft)
  obj <- vData_ft[[1]]
  
  dl_server("dl",
            dataIn = reactive({obj}),
            extension = c('csv', 'xlsx', 'RData')
  )
}

if(interactive())
  shinyApp(ui = ui, server = server)