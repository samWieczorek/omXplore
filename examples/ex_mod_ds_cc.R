library(highcharter)
library(shinyBS)
library(shiny)
library(DaparViz)


ui <- fluidPage(
  mod_ds_cc_ui("plot")
)

server <- function(input, output, session) {
  vList <- BuildExampleDataset('QFeatures')
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_cc_server("plot", reactive({vData}))
}

shinyApp(ui = ui, server = server)
