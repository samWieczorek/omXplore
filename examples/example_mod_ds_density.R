
library(SummarizedExperiment)
data(ft, package='DaparViz')

vizData <- Build_DaparVizData(ft,1)

densityPlot(vizData@qdata, conds = vizData@conds)


#------------------------------------------
# Shiny module
#------------------------------------------

data(ft, package='DaparViz')
ui <- shinyUI(mod_ds_density_ui("plot"))

server <- shinyServer(function(input, output, session) {
  mod_ds_density_server("plot", vizData = reactive({vizData})
  )
                         })

shinyApp(ui, server)
