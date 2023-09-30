library(shiny)
library(DaparViz)

#vList <- BuildExampleDataset('QFeatures')
vList <- BuildExampleDataset('MSnbase')
#vList <- BuildExampleDataset('list')
vData <- vList@ll.vizData[[1]]

corrMatrix(vData@qdata)


#------------------------------------------
# Shiny module
#------------------------------------------
ui <- fluidPage(
  mod_ds_corrmatrix_ui("plot")
)

server <- function(input, output, session) {
  #vList <- BuildExampleDataset('QFeatures')
  vList <- BuildExampleDataset('MSnbase')
  #vList <- BuildExampleDataset('list')
  vData <- vList@ll.vizData[[1]]
  
  # Example with a QFeatures dataset
  #
  data(Exp1_R25_pept, package='DaparToolshedData')
  vData <- Exp1_R25_pept
  
  
  mod_ds_corrmatrix_server("plot", reactive({vData}))
  }

if (interactive())
  shinyApp(ui = ui, server = server)
