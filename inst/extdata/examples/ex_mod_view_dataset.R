library(DaparViz)

options(shiny.fullstacktrace = TRUE)


ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
   
  data(vData_ft)
  obj <- vData_ft
  
  mod_view_dataset_server("dataset", 
                          ll.vizData = reactive({obj}),
                          addons = list())
}

if (interactive())
  shinyApp(ui, server)