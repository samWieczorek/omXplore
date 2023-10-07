library(DaparViz)

options(shiny.fullstacktrace = TRUE)

ui <- fluidPage(
  
  mod_plots_tracking_ui("tracker")
)



server <- function(input, output, session) {
  data(vData_ft)
  obj <- vData_ft[[1]]
  
  indices <- mod_plots_tracking_server("tracker", 
                                       obj = reactive({obj}))
  
  observe({
    print(indices())
    
  })
}

if (interactive())
  shinyApp(ui, server)