library(DaparViz)

#options(shiny.fullstacktrace = TRUE)


ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
   
  data(vData_ft)
  obj <- vData_ft
  
  mod_view_dataset_server("dataset", 
                          obj = reactive({obj}),
                          addons = list(DaparToolshed=c('mod_ds_metacell'))
                          )
}

if (interactive())
  shinyApp(ui, server)