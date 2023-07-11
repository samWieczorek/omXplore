

data(ft, package='DaparViz')

vizData <- Build_DaparVizData(ft,1)
corrMatrix(vizData@qdata)


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparViz')
    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
      qdata <- assay(ft, 1)
      mod_ds_corrmatrix_server("plot", reactive({vizData}))
    }

    shinyApp(ui = ui, server = server)
