library(MSnbase)
library(shiny)

vList <- BuildExampleDataset('MSnbase')
vData <- vList@ll.vizData[[1]]

densityPlot(vData@qdata, conds = vData@conds)


#------------------------------------------
# Shiny module
#------------------------------------------


ui <- fluidPage(mod_ds_density_ui("plot"))

server <- shinyServer(function(input, output, session) {
  
  
  ##
  vList <- BuildExampleDataset('MSnbase')
  vData <- vList@ll.vizData[[1]]
  
  
  # Example with a QFeatures dataset
  #
  data(ft)
  obj <- convert2viz(ft)@ll.vizData[[1]]
  
  
  mod_ds_density_server("plot", 
                        vizData = reactive({obj}))
  })

if (interactive())
  shinyApp(ui, server)
