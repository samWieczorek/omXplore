
library(SummarizedExperiment)
data(ft, package='DaparViz')
densityPlot(assay(ft, 1), conds = colData(ft)$Condition)


#------------------------------------------
# Shiny module
#------------------------------------------

data(ft, package='DaparViz')
ui <- shinyUI(mod_ds_density_ui("plot"))

server <- shinyServer(function(input, output, session) {
  mod_ds_density_server( "plot",
                         reactive({assay(ft, 1)}),
                         colData(ft)$Condition)
    })

shinyApp(ui, server)
