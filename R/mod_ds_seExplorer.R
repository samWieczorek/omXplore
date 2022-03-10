#' @title Explore a `SummarizedExperiment` object
#' 
#' @description 
#' xxxx
#' 
#' @name SE-explorer
#' 
#' @examples
#' library(QFeatures)
#' data(ft)
#' corrMatrix(assay(ft, 1))
#' 
#' 
#' #------------------------------------------
#' # Shiny module
#' #------------------------------------------
#' if(interactive()){
#' library(DaparToolshed)
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


#' @param id xxx
#' @export 
#' @importFrom shiny NS tagList 
#' @rdname SE-explorer
mod_ds_seExplorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyBS::bsCollapse(id = "collapseCCInfos", 
                        open = "",
                        multiple = TRUE,
                        shinyBS::bsCollapsePanel("Quantitative data", 
                                                 DT::DTOutput(ns("qdata_ui")),
                                                 style = 'info'),
                        shinyBS::bsCollapsePanel("Metadata", 
                                                 DT::DTOutput(ns("metadata_ui")),
                                                 style = 'info')
                        ),
    mod_colorLegend_ui(ns('legend'))
  )
}

#' @param id xxx
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
#' @rdname QFeatures-explorer
mod_ds_seExplorer_server <- function(id,
                                     se,
                                     digits = reactive({3})
                                     ){ 
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(se())
      stopifnot (inherits(se(), "SummarizedExperiment"))
    })

    tags <- unique(rowData(se())$qMetadata)
    mod_colorLegend_server('legend', 
                           tags, 
                           ExtendPalette(length(tags), 'Dark2'))
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
#                             options=list(initComplete = initComplete(),
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
      
      rdata <- rowData(se())
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
                                   columnDefs.targets = c(list(0),list(1),list(2)))
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
      df <- cbind(keyId = rowData(se())[, idcol(se())],
                  round(assay(se()), digits = digits()), 
                  qMetadata(se())
                  )
      mc <- qMetadata.def(typeDataset(se()))
      colors <- as.list(setNames(mc$color, mc$node))
      
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
        formatStyle(
          colnames(df)[2:(1 + (ncol(df)-1)/2)],
          colnames(df)[((2 + (ncol(df)-1)/2)):ncol(df)],
          backgroundColor = styleEqual(names(colors), unlist(colors)),
          backgroundSize = '98% 48%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    })
    

    getDataForExprs <- reactive({
      req(se())
      test.table <- round(assay(se()), digits=10)
      test.table <- tibble::as_tibble(test.table)
      
      addon.table <- matrix(rep(NA,
                                ncol(test.table) * nrow(test.table)), 
                            nrow = nrow(test.table)
                            )
      addon.table <- tibble::as_tibble(addon.table)
      test.table <- cbind(test.table, addon.table)

      test.table
    })
  
  })
  
  
  
}
