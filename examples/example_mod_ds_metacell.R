
library(SummarizedExperiment)
data(ft_na, package='DaparViz')
mvPerLinesHisto(assay(ft_na, 1))

mvPerLinesHistoPerCondition(assay(ft_na, 1), colData(ft_na)$Condition)
mvHisto(assay(ft_na, 1), colData(ft_na)$Condition)

#------------------------------------------
# Shiny module
#------------------------------------------

data(ft_na, package='DaparViz')

ui <- fluidPage(
  mod_ds_metacell_ui('test')
)

server <- function(input, output) {
  utils::data("Exp1_R25_prot", package='DaparToolshedData')
  obj <- Exp1_R25_prot
  pattern <- c('Missing POV', 'Missing MEC')
  
  # pattern <- NULL
  observe({
    mod_ds_metacell_server('test',
                           obj = reactive({obj[[1]]}),
                           design = design.qf(obj),
                           pal = reactive({NULL}),
                           pattern = reactive({pattern}),
                           showSelect = reactive({is.null(pattern)})
    )
  })
  
}

shinyApp(ui = ui, server = server)

