library(DaparViz)
library(highcharter)

options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  mod_view_dataset_ui("dataset")
)



server <- function(input, output, session) {
  data(ft, package='DaparToolshed')
  data(ft_na, package='DaparToolshed')
  
  vList <- convert2viz(ft)
  vList_na <- convert2viz(ft_na)
  
  
  mod_view_dataset_server("dataset", 
                          ll.vizData = reactive({vList}))
}

shinyApp(ui, server)