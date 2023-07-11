
library(SummarizedExperiment)
data(ft, package='DaparViz')

vizData <- Build_DaparVizData(ft, 1)
violinPlot(vizData@qdata, vizData@conds)
violinPlot(vizData@qdata, vizData@conds, subset = c(2, 4))

boxPlot(vizData@qdata, vizData@conds)
boxPlot(vizData@qdata, vizData@conds, subset = c(2, 4))


#------------------------------------------
# Shiny module
#------------------------------------------
data(ft, package='DaparViz')

ui <- mod_ds_intensity_ui("iplot")

server <- function(input, output, session) {
  vizData <- Build_DaparVizData(ft, 1)
  mod_ds_intensity_server("iplot",
                          vizData = reactive({vizData})
                          )
    }

    shinyApp(ui = ui, server = server)