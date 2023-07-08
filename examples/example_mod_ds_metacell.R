
library(SummarizedExperiment)
data(ft_na, package='DaparViz')
mvPerLinesHisto(assay(ft_na, 1))

mvPerLinesHistoPerCondition(assay(ft_na, 1), colData(ft_na)$Condition)
mvHisto(assay(ft_na, 1), colData(ft_na)$Condition)

#------------------------------------------
# Shiny module
#------------------------------------------

data(ft_na, package='DaparViz')
ui <- mod_ds_mv_ui("plot")

server <- function(input, output, session) {
  mod_ds_mv_server("plot",
                   se = reactive({assay(ft_na, 1)}),
                   design = design(ft_na))
    }

shinyApp(ui = ui, server = server)
