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
  data(Exp1_R25_pept, package='DaparToolshedData')
  obj <- convert2viz(Exp1_R25_pept)@ll.vizData[[1]]
  
  
  mod_ds_density_server("plot", 
                        vizData = reactive({obj}))
  })

if (interactive())
  shinyApp(ui, server)
