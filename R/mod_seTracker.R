#' @title  Tracking of entities within plots
#'
#' @description 
#' 
#' This shiny module offers a UI to select a subset of a dataset
#' and superimpose quantitative values of this selection on the complete plot
#' Three modes of selection are implemented:
#'
#' - 'Protein list': xxx,
#' - 'Random': xxx,
#' - 'Specific column': xxx
#' 
#' @name tracking
#'
#' @examples 
#' if (interactive()){
#' 
#' ui <- tagList(
#'   mod_seTracker_ui('track'),
#'   uiOutput('show')
#' )
#' 
#' server <- function(input, output, session) {
#'   tmp <- reactiveVal()
#'   
#'   tmp <- mod_seTracker_server(id = 'track',
#'                               se = reactive({ft[[1]]})
#'   )
#'   
#'   output$show <- renderUI({
#'     p(paste0(tmp(), collapse = ' '))
#'   })
#'   
#' }
#' shinyApp(ui=ui, server=server)
#' }
NULL

 
#' @param id shiny id
#' @export
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs hidden
#' 
#' @rdname tracking
#'
mod_seTracker_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    actionButton(ns('rst_btn'), 'Reset'),
    uiOutput(ns('typeSelect_ui')),
    uiOutput(ns('listSelect_ui')),
    uiOutput(ns('randSelect_ui')),
    uiOutput(ns('colSelect_ui'))
  )
}

#' @param id xxx
#' @param se A instance of the class `SummarizedExperiment`
#' 
#' @rdname tracking
#'
#' @export
#'
#' @importFrom shinyjs toggle hidden show hide
#' @importFrom stats setNames
mod_seTracker_server <- function(id, se){
  if (! requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Please install SummarizedExperiment: BiocManager::install('SummarizedExperiment')")
  }
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rv.track <- reactiveValues(
      typeSelect = "None",
      listSelect = NULL,
      randSelect = '',
      colSelect = NULL,
      indices = NULL)

    
    output$typeSelect_ui <- renderUI({
      tmp <- c("None", "ProteinList", "Random", "Column")
      nm <- c("None", "Protein list", "Random", "Specific Column")
      
      selectInput(ns("typeSelect"), 
                  "Type of selection",
                  choices = setNames(tmp, nm),
                  selected = rv.track$typeSelect,
                  width = '130px')
    })

    
    output$listSelect_ui <- renderUI({
      widget <- selectInput(ns("listSelect"),
                  "Select protein",
                  choices = c('None', 
                              SummarizedExperiment::rowData(se())[, DaparToolshed::idcol(se())]),
                  multiple = TRUE,
                  selected = rv.track$listSelect,
                  width='200px',
                  #size = 10,
                  selectize = TRUE
                  )
      
      if (rv.track$typeSelect == 'ProteinList')
        widget
      else
        hidden(widget)
    })
    
    output$colSelect_ui <- renderUI({
      widget <- selectInput(ns("colSelect"),
                  "Column of rowData",
                  choices = c('', colnames(SummarizedExperiment::rowData(se()))),
                  selected = rv.track$colSelect
                  )
      if (rv.track$typeSelect == 'Column')
        widget
      else
        hidden(widget)
    })
    
    output$randSelect_ui <- renderUI({
      widget <- textInput(ns("randSelect"),  
                "Random", 
                value = rv.track$randSelect,
                width = ('120px'))
      if (rv.track$typeSelect == 'Random')
        widget
      else
        hidden(widget)
    })
    
    observeEvent(req(input$typeSelect),{
      rv.track$typeSelect <- input$typeSelect
    })


    observeEvent(input$rst_btn, {
      rv.track$typeSelect <- "None"
      rv.track$listSelect <- NULL
      rv.track$randSelect <- ''
      rv.track$colSelect <- NULL
      rv.track$indices <- NULL
    })

    # Catch event on the list selection
    observeEvent(input$listSelect,{
      rv.track$listSelect <- input$listSelect
      rv.track$randSelect <- ''
      rv.track$colSelect <- ''

      if(is.null(rv.track$listSelect))
        rv.track$indices <- NULL
      else
        rv.track$indices <-  match(rv.track$listSelect, 
                                   SummarizedExperiment::rowData(se())[,DaparToolshed::idcol(se())])
    })





    observeEvent(input$randSelect, {
      rv.track$randSelect <- input$randSelect
      rv.track$listSelect  <- NULL
      rv.track$colSelect <- NULL
      cond <- is.null(rv.track$randSelect) 
      cond <- cond || rv.track$randSelect == '' 
      cond <- cond || (as.numeric(rv.track$randSelect) < 0)
      cond <- cond || (as.numeric(rv.track$randSelect) > nrow(se()))
      if (!cond)
        rv.track$indices <- sample(seq_len(nrow(se())), 
                                   as.numeric(rv.track$randSelect), 
                                   replace = FALSE)

    })

    observeEvent(input$colSelect, {
      rv.track$colSelect <- input$colSelect
      rv.track$listSelect <- NULL
      rv.track$randSelect <- ''

      if (rv.track$colSelect != '')
        rv.track$indices <- which(SummarizedExperiment::rowData(se())[,rv.track$colSelect] == 1)
     })

    return(reactive({rv.track$indices}))

  })

}

