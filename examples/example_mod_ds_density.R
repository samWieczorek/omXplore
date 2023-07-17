
library(SummarizedExperiment)
data(ft, package='DaparToolshed')
vList <- convert2viz(ft)
vData <- vList@ll.vizData[[1]]

densityPlot(vData@qdata, conds = vData@conds)


#------------------------------------------
# Shiny module
#------------------------------------------


ui <- shinyUI(mod_ds_density_ui("plot"))

server <- shinyServer(function(input, output, session) {
  data(ft, package='DaparToolshed')
  
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  mod_ds_density_server("plot",
                        vizData = reactive({vData})
  )
                         })

shinyApp(ui, server)
