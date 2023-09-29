library(MSnbase)
#library(DaparToolshed)
library(highcharter)
library(DaparViz)
library(shiny)



ui <- fluidPage(
  mod_ds_seExplorer_ui("plot")
)

server <- function(input, output, session) {
  data(ft)
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_seExplorer_server("plot", reactive({vData}))
  
    }

if (interactive())
  shinyApp(ui = ui, server = server)