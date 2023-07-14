library(MSnbase)
library(DaparToolshed)
library(highcharter)
library(DaparViz)


data(ft, package='DaparToolshed')
vizData <- Coerce2VizData(ft, 1)

ui <- mod_ds_seExplorer_ui("plot")

server <- function(input, output, session) {
        mod_ds_seExplorer_server("plot", reactive({vizData})
        )
    }

shinyApp(ui = ui, server = server)