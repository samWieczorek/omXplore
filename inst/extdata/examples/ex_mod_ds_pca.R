
library(DaparViz)

ui <- mod_ds_pca_ui("plot")

server <- function(input, output, session) {
  data(vData_ms)
  obj <- vData_ms[[1]]
  
  mod_ds_pca_server("plot", 
                    vizData = reactive({obj}))
  }

if (interactive())
  shinyApp(ui = ui, server = server)
