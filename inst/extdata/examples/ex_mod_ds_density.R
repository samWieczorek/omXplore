library(DaparViz)


ui <- fluidPage(mod_ds_density_ui("plot"))

server <- shinyServer(function(input, output, session) {
  data(vData_ft)
  obj <- vData_ft[1]
  mod_ds_density_server("plot", 
                        vizData = reactive({obj}))
  })

if (interactive())
  shinyApp(ui, server)
