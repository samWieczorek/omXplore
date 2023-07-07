#-------------------------------------------------------------------
ui <- tagList(
  mod_colorLegend_ui("plot1"),
  mod_colorLegend_ui("plot2"),
  mod_colorLegend_ui("plot3")
  )

server <- function(input, output, session) {

      data(Exp1_R25_prot, package='DaparToolshedData')

      # Use the default color palette
      mod_colorLegend_server("plot1", Exp1_R25_prot[[1]])

      # Use of a user-defined color palette
      mod_colorLegend_server("plot2", Exp1_R25_prot[[1]])

      # Use of a  palette
      mod_colorLegend_server("plot3", Exp1_R25_prot[[1]])
  }

shinyApp(ui, server)