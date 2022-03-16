#' @title Explore a `SummarizedExperiment` object
#' 
#' @description 
#' xxxx
#' 
#' @name SE-explorer
#' 
#' @examples
#' data(ft)
#' corrMatrix(assay(ft, 1))
#' 
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#' data(ft_na)
#'  ui <- mod_ds_seExplorer_ui('plot')
#' 
#'  server <- function(input, output, session) {
#'   mod_ds_seExplorer_server('plot', 
#'   reactive({ft_na[[1]]})
#'   )}
#'  
#'  shinyApp(ui=ui, server=server)
#' }
NULL


#' @param id A `character(1)` which is the id of the shiny module.
#' @export 
#' @importFrom shiny NS tagList 
#' @rdname SE-explorer
mod_ds_seExplorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyBS::bsCollapse(id = "infos", 
                        open = "",
                        multiple = TRUE,
                        shinyBS::bsCollapsePanel("Quantitative data", 
                                                 DT::DTOutput(ns("qdata_ui")),
                                                 style = 'info'),
                        shinyBS::bsCollapsePanel("Metadata", 
                                                 DT::DTOutput(ns("metadata_ui")),
                                                 style = 'info'),
                        shinyBS::bsCollapsePanel("quantitative Metadata", 
                                                 DT::DTOutput(ns("qMetadata_ui")),
                                                 style = 'info')
                        ),
    mod_colorLegend_ui(ns('legend'))
  )
}

#' @param id A `character(1)` which is the id of the shiny module.
#' @param se An instance of the class `SummarizedExperiment`
#' @param digits xxx
#' 
#' 
#' @export
#' 
#' @return NA
#' @importFrom DT formatStyle styleEqual datatable
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' 
#' @rdname SE-explorer
mod_ds_seExplorer_server <- function(id,
                                     se,
                                     digits = reactive({3})
                                     ){ 
  
  if (! requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Please install SummarizedExperiment: BiocManager::install('SummarizedExperiment')")
  }
  
  if (! requireNamespace("DaparToolshed", quietly = TRUE)) {
    stop("Please install DaparToolshed: BiocManager::install('DaparToolshed')")
  }
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(se())
      stopifnot (inherits(se(), "SummarizedExperiment"))
      tmp.tags <- DaparToolshed::custom_qMetadata_colors()
      mod_colorLegend_server('legend', 
                             text = names(tmp.tags), 
                             colors = unname(unlist(tmp.tags))
                             )
    })

    
# 
#     output$viewDesign <- DT::renderDT({
#       req(se())
#       
#       data <- tibble::as_tibble(colData(se()))
#       
#       pal <- unique(RColorBrewer::brewer.pal(8, "Dark2"))
#       
#       dt <- DT::datatable(  data,
#                             extensions = c('Scroller', 'Buttons'),
#                             rownames=  FALSE,
#                             options=list(initComplete = .initComplete(),
#                                          dom = 'Brtip',
#                                          pageLength=10,
#                                          orderClasses = TRUE,
#                                          autoWidth=TRUE,
#                                          deferRender = TRUE,
#                                          bLengthChange = FALSE,
#                                          scrollX = 200,
#                                          scrollY = 500,
#                                          scroller = TRUE,
#                                          columnDefs = list(list(width='60px',targets= "_all"))
#                             )) %>%
#         DT::formatStyle(
#           columns = colnames(data)[seq_len(2)],
#           valueColumns = colnames(data)[2],
#           backgroundColor = DT::styleEqual(unique(data$Condition), 
#                                            pal[seq_len(length(unique(data$Condition)))])
#         )
#       
#     })
    
    
    output$metadata_ui <- DT::renderDT({
      req(se())
      
      rdata <- SummarizedExperiment::rowData(se())
      # Delete columns that are not one-dimensional
      rdata <- rdata[, - which(colnames(rdata) == 'adjacencyMatrix')]
      rdata <- rdata[, - which(colnames(rdata) == 'qMetadata')]
      
      dat <- DT::datatable(tibble::as_tibble(rdata),
                             rownames = TRUE,
                             extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                             options=list(
                               initComplete = .initComplete(),
                               dom = 'Bfrtip',
                               pageLength  = 10,
                               deferRender = TRUE,
                               bLengthChange = FALSE,
                               scrollX = 200,
                               scrollY = 600,
                               scroller = TRUE,
                               orderClasses = TRUE,
                               autoWidth = FALSE,
                               columns.searchable = FALSE,
                               fixedColumns = list(
                                 leftColumns = 1
                                 ),
                               columnDefs = list(
                                 list(
                                   columns.width = c("60px"),
                                   targets = c(list(0),list(1),list(2)))
                                 )
                               )
                           )

      if ('Significant' %in% colnames(rdata))
        dat <- dat %>%
        DT::formatStyle(columns = 'Significant',
                        target = 'row',
                        background = DT::styleEqual(1, 'lightblue'))
      
      return(dat)
    })
    
    
    output$qdata_ui <- DT::renderDataTable(server=TRUE,{
      req(se())
      df <- cbind(keyId = SummarizedExperiment::rowData(se())[, DaparToolshed::idcol(se())],
                  round(SummarizedExperiment::assay(se()), 
                        digits = digits()), 
                  DaparToolshed::qMetadata(se())
                  )
      colors <- DaparToolshed::custom_qMetadata_colors()
      
      DT::datatable( df,
                     extensions = c('Scroller'),
                     options = list(
                       initComplete = .initComplete(),
                       displayLength = 20,
                       deferRender = TRUE,
                       bLengthChange = FALSE,
                       scrollX = 200,
                       scrollY = 600,
                       scroller = TRUE,
                       ordering = FALSE,
                       server = TRUE,
                       columnDefs = list(
                         list(
                           targets = c((( 2 + (ncol(df)-1)/2)):ncol(df)), 
                           visible = FALSE)
                         )
                       )
                     ) %>%
        DT::formatStyle(
          colnames(df)[2:(1 + (ncol(df)-1)/2)],
          colnames(df)[((2 + (ncol(df)-1)/2)):ncol(df)],
          backgroundColor = DT::styleEqual(names(colors), 
                                           unname(unlist(colors))
                                           ),
          backgroundSize = '98% 48%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    })
    
    output$qMetadata_ui <- DT::renderDataTable(server=TRUE,{
      req(se())
      df <- DaparToolshed::qMetadata(se())

      colors <- DaparToolshed::custom_qMetadata_colors()
      
      DT::datatable( df,
                     extensions = c('Scroller'),
                     options = list(
                       initComplete = .initComplete(),
                       displayLength = 20,
                       deferRender = TRUE,
                       bLengthChange = FALSE,
                       scrollX = 200,
                       scrollY = 600,
                       scroller = TRUE,
                       ordering = FALSE,
                       server = TRUE
                     )
      ) %>%
        DT::formatStyle(
          colnames(df),
          colnames(df),
          backgroundColor = DT::styleEqual(names(colors), 
                                           unname(unlist(colors))
                                           ),
          backgroundSize = '98% 48%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    })
  })
  
  
  
}
