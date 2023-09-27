library(shiny)


vList <- BuildExampleDataset('MSnbase')
vData <- vList@ll.vizData[[1]]


violinPlot(vData@qdata, vData@conds)
violinPlot(vData@qdata, vData@conds, subset = c(2, 4))

boxPlot(vData@qdata, vData@conds)
boxPlot(vData@qdata, vData@conds, subset = c(2, 4))


#------------------------------------------
# Shiny module
#------------------------------------------

ui <- fluidPage(
  tagList(
    mod_plots_tracking_ui('tracker'),
    mod_ds_intensity_ui("iplot")
  )
)

server <- function(input, output, session) {
  data(ft)
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  indices <- mod_plots_tracking_server("tracker", 
                                       vizData = reactive({vData}))
  
  mod_ds_intensity_server("iplot",
                          vizData = reactive({vData}),
                          track.indices = reactive({indices()})
                          )
    }

if (interactive())
  shinyApp(ui = ui, server = server)