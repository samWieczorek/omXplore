
library(SummarizedExperiment)
data(ft, package='DaparViz')
corrMatrix(assay(ft, 1))


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparViz')
    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
      qdata <- assay(ft, 1)
      mod_ds_corrmatrix_server("plot", reactive({qdata}))
    }

    shinyApp(ui = ui, server = server)
