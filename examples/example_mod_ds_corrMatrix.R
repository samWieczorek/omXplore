

data(ft, package='DaparToolshed')
vList <- convert2viz(ft)
vData <- vList@ll.vizData[[1]]

corrMatrix(vData@qdata)


#------------------------------------------
# Shiny module
#------------------------------------------

    data(ft, package='DaparToolshed')
    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
      data(ft, package='DaparToolshed')
      
      vList <- convert2viz(ft)
      vData <- vList@ll.vizData[[1]]
      
      mod_ds_corrmatrix_server("plot", reactive({vData}))
    }

    shinyApp(ui = ui, server = server)
