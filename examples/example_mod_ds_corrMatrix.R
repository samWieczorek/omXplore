

data(ft, package='DaparToolshed')

vizData <- convert2viz(ft)
corrMatrix(vizData[[1]]@qdata)


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparToolshed')
    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
      mod_ds_corrmatrix_server("plot", reactive({vizData[[1]]}))
    }

    shinyApp(ui = ui, server = server)
