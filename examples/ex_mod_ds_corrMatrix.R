

#vList <- BuildExampleDataset('QFeatures')
vList <- BuildExampleDataset('MSnbase')
#vList <- BuildExampleDataset('list')
vData <- vList@ll.vizData[[1]]

corrMatrix(vData@qdata)


#------------------------------------------
# Shiny module
#------------------------------------------


    ui <- mod_ds_corrmatrix_ui("plot")

    server <- function(input, output, session) {
      #vList <- BuildExampleDataset('QFeatures')
      vList <- BuildExampleDataset('MSnbase')
      #vList <- BuildExampleDataset('list')
      vData <- vList@ll.vizData[[1]]
      
      mod_ds_corrmatrix_server("plot", reactive({vData}))
    }

    shinyApp(ui = ui, server = server)
