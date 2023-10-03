library(DaparViz)

ui <- mod_format_DT_ui("dt")

server <- function(input, output, session) {
  
  rv <- reactiveValues(selected = NULL)

  data(vData_ft)
  obj <- vData_ft[[1]]
  hcStyle = list(
    cols = colnames(obj@qdata)[1:6],
    vals = colnames(obj@qdata)[4],
    unique = xxxx,
    pal = xxx
  )
  
  rv$selected <- mod_format_DT_server("dt", data = reactive({obj@qdata}) )
  
   observeEvent(rv$selected(), {
     print(paste0('Selected line(s):', rv$selected()))
   })
}

if (interactive())
  shinyApp(ui = ui, server = server)
