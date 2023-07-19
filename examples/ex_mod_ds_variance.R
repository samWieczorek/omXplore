library(highcharter)
library(DaparViz)
data(ft, package='DaparToolshed')
vList <- convert2viz(ft)
vData <- vList@ll.vizData[[1]]


CVDist(vData)


#------------------------------------------
# Shiny module
#------------------------------------------

ui <- shinyUI(mod_ds_variance_ui("plot"))

server <- shinyServer(function(input, output, session) {
  data(ft, package='DaparToolshed')
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_variance_server("plot", vData = reactive({vData}))
})

shinyApp(ui, server)