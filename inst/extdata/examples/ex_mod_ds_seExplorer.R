library(DaparViz)


ui <- fluidPage(
  mod_ds_seExplorer_ui("plot")
)

server <- function(input, output, session) {
  data(vData_ft)
  obj <- vData_ft[1]
  
  mod_ds_seExplorer_server("plot", reactive({obj}))
  
    }

if (interactive())
  shinyApp(ui = ui, server = server)