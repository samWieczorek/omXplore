library(SummarizedExperiment)
data(ft, package='DaparToolshed')

ll.vizData <- Coerce2VizData(ft)

CVDist(ll.vizData[[1]])


#------------------------------------------
# Shiny module
#------------------------------------------

ui <- shinyUI(mod_ds_variance_ui("plot"))

server <- shinyServer(function(input, output, session) {
  mod_ds_variance_server("plot", vizData = reactive({ll.vizData[[1]]})
  )
})

shinyApp(ui, server)