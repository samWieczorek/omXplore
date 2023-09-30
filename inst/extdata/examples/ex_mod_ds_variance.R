library(highcharter)
library(DaparViz)
library(shiny)


data(ft)
vList <- convert2viz(ft)
vData <- vList@ll.vizData[[1]]


CVDist(vData)


#------------------------------------------
# Shiny module
#------------------------------------------

ui <- fluidPage(mod_ds_variance_ui("plot"))

server <- shinyServer(function(input, output, session) {
  data(ft)
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  #vData <- ft
  
  mod_ds_variance_server("plot", vizData = reactive({vData}))
})

if (interactive())
  shinyApp(ui, server)