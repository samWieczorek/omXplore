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
  
  data(vData_ft)
  obj <- vData_ft[1]
  
 tags <- GetMetacellTags(obj@metacell, 
                          level = obj@type, 
                          onlyPresent = TRUE)
  
  # Use the default color palette
  mod_colorLegend_server("plot1", tags)

  # Use of a user-defined color palette
  mod_colorLegend_server("plot2", tags)

  # Use of a  palette
  mod_colorLegend_server("plot3", tags)
  }

if (interactive())
  shinyApp(ui, server)