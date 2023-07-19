
vList <- BuildExampleDataset('MSnbase')
vData <- vList@ll.vizData[[1]]
heatmapD(vData)
mv.heatmap(vData@qdata)

#------------------------------------------
# Shiny module
#------------------------------------------


ui <- mod_ds_heatmap_ui("plot")

server <- function(input, output, session) {
  
  vList <- BuildExampleDataset('MSnbase')
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_heatmap_server("plot",
                        reactive({vData})
  )
  }
shinyApp(ui = ui, server = server)
