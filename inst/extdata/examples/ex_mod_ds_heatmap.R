library(shiny)

vList <- BuildExampleDataset('MSnbase')
vData <- vList@ll.vizData[[1]]
heatmapD(vData)
mv.heatmap(vData@qdata)

#------------------------------------------
# Shiny module
#------------------------------------------


ui <- fluidPage(
  mod_ds_heatmap_ui("plot")
)

server <- function(input, output, session) {
  
  vList <- BuildExampleDataset('MSnbase')
  vData <- vList@ll.vizData[[1]]
  
  data(ft)
  vData <- ft
  
  mod_ds_heatmap_server("plot", reactive({vData})
  )
  }

if (interactive())
  shinyApp(ui = ui, server = server)
