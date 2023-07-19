
# Example
#
ui <- fluidPage(
  tagList(
    mod_metacell_tree_ui('tree'),
    uiOutput('res')
  )
)

server <- function(input, output) {
  rv <- reactiveValues(
    tags = NULL
  )
  
  observe({
    rv$tags <- mod_metacell_tree_server('tree', type = 'peptide')
  })
  
  
}

shinyApp(ui = ui, server = server)
