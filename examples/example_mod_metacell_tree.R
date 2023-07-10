


#=============================================================================


# Example
#
ui <- fluidPage(
  tagList(
    mod_metacell_tree_ui('tree'),
    uiOutput('res')
  )
)

server <- function(input, output) {
  mod_metacell_tree_server('tree', type = 'peptide')
}

shinyApp(ui = ui, server = server)
