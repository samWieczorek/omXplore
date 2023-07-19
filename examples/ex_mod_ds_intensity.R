
vList <- BuildExampleDataset('MSnbase')
vData <- vList@ll.vizData[[1]]


violinPlot(vData@qdata, vData@conds)
violinPlot(vData@qdata, vData@conds, subset = c(2, 4))

boxPlot(vData@qdata, vData@conds)
boxPlot(vData@qdata, vData@conds, subset = c(2, 4))


#------------------------------------------
# Shiny module
#------------------------------------------

ui <- mod_ds_intensity_ui("iplot")

server <- function(input, output, session) {
  data(ft, package='DaparToolshed')
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  
  mod_ds_intensity_server("iplot",
                          vizData = reactive({vData})
                          )
    }

    shinyApp(ui = ui, server = server)