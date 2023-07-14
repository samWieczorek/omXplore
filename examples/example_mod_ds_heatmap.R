
library(SummarizedExperiment)
data(ft_na, package='DaparViz')

vizData <- Coerce2VizData(ft,1)

heatmapD(vizData@qdata, vizData@conds)
mv.heatmap(vizData@qdata)

#------------------------------------------
# Shiny module
#------------------------------------------

data(ft, package='DaparViz')
ui <- mod_ds_heatmap_ui("plot")

server <- function(input, output, session) {
  vizData <- Coerce2VizData(ft,1)
  
  mod_ds_heatmap_server("plot",
                        reactive({vizData})
  )
  }
shinyApp(ui = ui, server = server)
