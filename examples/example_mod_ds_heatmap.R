
library(SummarizedExperiment)
data(ft_na, package='DaparViz')
heatmapD(assay(ft_na, 1), colData(ft_na)$Condition)
mv.heatmap(assay(ft_na, 1))

#------------------------------------------
# Shiny module
#------------------------------------------

data(ft, package='DaparViz')
ui <- mod_ds_heatmap_ui("plot")

server <- function(input, output, session) {
  mod_ds_heatmap_server("plot",
                        reactive({assay(ft, 1)}),
                        colData(ft)$Condition)
  }
shinyApp(ui = ui, server = server)
