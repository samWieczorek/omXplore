library(MSnbase)
library(DaparToolshed)
library(SummarizedExperiment)
library(highcharter)
library(shinyBS)

data(ft, package='DaparToolshed')
#------------------------------------------
# Shiny module
#------------------------------------------

ui <- fluidPage(
  mod_ds_metacell_ui('test')
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    tags = NULL
  )
  vList <- convert2viz(ft)
  vData <- vList@ll.vizData[[1]]
  #pattern <- c('Missing POV', 'Missing MEC')
   pattern <- NULL
   
  observe({
    rv$tags <- mod_ds_metacell_server('test',
                           vizData = reactive({vData}),
                           pal = reactive({NULL}),
                           pattern = reactive({pattern}),
                           showSelect = reactive({is.null(pattern)})
    )
  })
  
}

shinyApp(ui = ui, server = server)

