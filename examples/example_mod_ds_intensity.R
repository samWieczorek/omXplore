
library(SummarizedExperiment)
data(ft, package='DaparViz')
conds <- colData(ft)$Condition
violinPlot(assay(ft, 1), conds)
violinPlot(assay(ft, 1), conds, subset = c(2, 4))

boxPlot(assay(ft, 1), conds)
boxPlot(assay(ft, 1), conds, subset = c(2, 4))


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparViz')
    ui <- mod_ds_intensity_ui("iplot")

    server <- function(input, output, session) {
        mod_ds_intensity_server("iplot", reactive({ft[[1]]}),
            colData(ft)$Condition)
    }

    shinyApp(ui = ui, server = server)