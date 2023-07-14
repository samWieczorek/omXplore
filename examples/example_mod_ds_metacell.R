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
  vizData <- Coerce2VizData(ft)
  #pattern <- c('Missing POV', 'Missing MEC')
   pattern <- NULL
   
  observe({
    mod_ds_metacell_server('test',
                           vizData = reactive({vizData[[1]]}),
                           pal = reactive({NULL}),
                           pattern = reactive({pattern}),
                           showSelect = reactive({is.null(pattern)})
    )
  })
  
}

shinyApp(ui = ui, server = server)

