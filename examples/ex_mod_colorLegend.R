library(DaparToolshed)
library(DaparToolshedData)
library(highcharter)
library(shinyBS)
library(DaparViz)

#-------------------------------------------------------------------
ui <- fluidPage(
  tagList(
  mod_colorLegend_ui("plot1"),
  mod_colorLegend_ui("plot2"),
  mod_colorLegend_ui("plot3")
  )
)

server <- function(input, output, session) {
  
  data(Exp1_R25_prot, package='DaparToolshedData')
  vList <- convert2viz(Exp1_R25_prot)
  vData <- vList@ll.vizData[[1]]
  
 tags <- GetMetacellTags(vData@metacell, 
                          level = vData@type, 
                          onlyPresent = TRUE)
     
  # Use the default color palette
  mod_colorLegend_server("plot1", tags)

  # Use of a user-defined color palette
  mod_colorLegend_server("plot2", tags)

  # Use of a  palette
  mod_colorLegend_server("plot3", tags)
  }

shinyApp(ui, server)