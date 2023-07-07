
library(SummarizedExperiment)
data(ft, package='DaparViz')
corrMatrix(assay(ft, 1))


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparViz')
    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
        mod_ds_corrmatrix_server(
            "plot",
            reactive({assay(ft, 1)})
        )
    }

    shinyApp(ui = ui, server = server)
