library(MSnbase)
library(DaparToolshed)
library(highcharter)
library(DaparViz)



ui <- fluidPage(
  mod_ds_seExplorer_ui("plot")
)

server <- function(input, output, session) {
  data(ft, package='DaparToolshed')
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_seExplorer_server("plot", reactive({vData}))
  
    }

shinyApp(ui = ui, server = server)