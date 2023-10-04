#' @title mod_plots_cc_ui and mod_plots_cc_server
#'
#' @description  A shiny Module.
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param DaparViz An instance of `DaparViz` class
#'
#' @keywords internal
#'
#' @return NA
#'
#' @name connected_components
#'
#'
NULL



#' @importFrom shiny NS tagList
#' @importFrom shinyjs toggle hidden
#' @import shinyBS
#'
#' @export
#' @rdname connected_components
#' 
mod_ds_cc_ui <- function(id) {
  pkgs.require(c('visNetwork', 'shinyBS', 'shinyjs'))
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      div(id = ns('badFormatMsg'), 
          h3('Dataset in not in correct format.')
      )
    ),
  tabPanel("Peptide-Protein Graph",
           value = "graphTab",
           tabsetPanel(id = "graphsPanel",
             
                       #---------------------------------------------------------
                       tabPanel("One-One Connected Components",
               tagList(
                 fluidRow(
                   column(width = 4, tagList(
                     dl_ui(ns("OneOneDT_DL_btns")),
                     uiOutput(ns("OneOneDT_UI"))
                   )),
                   column(width = 8, uiOutput(ns("OneOneDTDetailed_UI"))
                   )
                 )
               )
             ),
             
             #---------------------------------------------------------
             tabPanel("One-Multi Connected Components",
               tagList(
                 fluidRow(
                   column(width = 4,
                     tagList(
                       dl_ui(ns("OneMultiDT_DL_btns")),
                       uiOutput(ns("OneMultiDT_UI"))
                     )
                   ),
                   column(width = 8, uiOutput(ns("OneMultiDTDetailed_UI"))
                   )
                 )
               )
             ),
             
             #---------------------------------------------------------
             tabPanel("Multi-Multi Connected Components",
               tagList(
                 uiOutput(ns("pepInfo_ui")),
                 selectInput(ns("searchCC"), "Search for CC",
                             choices = c("Tabular view" = "tabular", "Graphical view" = "graphical"),
                             width = "150px"
                             ),
                 fluidRow(
                   column(width = 6, tagList(
                     highchartOutput(ns("jiji")),
                     uiOutput(ns("CCMultiMulti_DL_btns_ui")),
                     shinyjs::hidden(uiOutput(ns("CCMultiMulti_UI")))
                   )),
                   column(width = 6, visNetwork::visNetworkOutput(ns("visNetCC"), height = "600px"))
                 ),
                 uiOutput(ns("CCDetailed"))
               )
             )
           )
  )
  )
}



#' @importFrom tibble tibble
#' @importFrom shinyjs toggle hidden
#' @import highcharter
#' @export
#' @rdname connected_components
mod_ds_cc_server <- function(id, DaparViz) {
  pkgs.require(c('visNetwork', 'highcharter'))
  moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      rv <- reactiveValues(
        data = NULL
      )
      
      
      rvCC <- reactiveValues(
        ## selected CC in global CC list (tab or plot)
        selectedCC = NULL,
        selectedNode = NULL,
        selectedNeighbors = NULL,
        selectedCCgraph = NULL,
        
        # when the user selects a node in the graph
        detailedselectedNode = list(
          sharedPepLabels = NULL,
          specPepLabels = NULL,
          protLabels = NULL
        ),
        
        
        OneOneDT_rows_selected = reactive({NULL}),
        OneMultiDT_rows_selected = reactive({NULL}),
        CCMultiMulti_rows_selected = reactive({NULL})
      )
      
      observe({
        if(inherits(DaparViz(), "DaparViz"))
          rv$data <- DaparViz()
        
        shinyjs::toggle('badFormatMsg', condition = !inherits(DaparViz(), "DaparViz"))
      }, priority = 1000)
      
      ##//////////////////////////////////////////////////////////////////
      ##
      ##    One One Connected Components
      ##
      ##//////////////////////////////////////////////////////////////////
      observeEvent(req(input$searchCC), {
        
        shinyjs::toggle("jiji", condition = input$searchCC == "graphical")
        shinyjs::toggle("CCMultiMulti_UI", condition = input$searchCC == "tabular")
      })
      
      
      output$pepInfo_ui <- renderUI({
        selectInput(ns("pepInfo"), "Peptide Info",
                    choices = colnames(DaparViz()@metadata),
                    multiple = TRUE)
      })
      
      
      # select a point in the grpah
      observeEvent(input$click, { rvCC$selectedNode <- input$click})
      
      
      # Get the id of selected neighbors in the graph
      observeEvent(input$visNetCC_highlight_color_id, {
        rvCC$selectedNeighbors <- input$visNetCC_highlight_color_id
      })
      
      
      ##//////////////////////////////////////////////////////////////////
      ##
      ##    One Multi Connected Components
      ##
      ##//////////////////////////////////////////////////////////////////
      
      # select a CC in the jitter plot
      observeEvent(req(input$eventPointClicked), {
        .str <- strsplit(input$eventPointClicked, "_")
        this.index <- as.integer(.str[[1]][1])
        rvCC$selectedCC <- this.index + 1
      })
      
      
      output$visNetCC <- visNetwork::renderVisNetwork({
        
        req(rvCC$selectedCC)
        local <- (rv$data@cc)[Get_CC_Multi2Any()]
        #X <- rv$data@adjMat
        rvCC$selectedCCgraph <- buildGraph(local[[rvCC$selectedCC]])
        
        display.CC.visNet(rvCC$selectedCCgraph) %>%
          visEvents(click = paste0("function(nodes){Shiny.onInputChange('",
            ns("click"), "', nodes.nodes[0]);
                Shiny.onInputChange('", ns("node_selected"), "', nodes.nodes.length);;}"
          )) %>%
          visOptions(highlightNearest = TRUE)
      })
      
      
      
      
      
      
      
      ##//////////////////////////////////////////////////////////////////
      ##
      ##    Multi Multi Connected Components
      ##
      ##//////////////////////////////////////////////////////////////////
      
      
      
      output$jiji <- highcharter::renderHighchart({
        tooltip <- NULL
        
        isolate({
          local <- (rv$data@cc)[Get_CC_Multi2Any()]
          n.prot <- unlist(lapply(local, function(x) {ncol(x)}))
          n.pept <- unlist(lapply(local, function(x) {nrow(x)}))
          df <- tibble::tibble(x = jitter(n.pept),
                               y = jitter(n.prot),
                               index = 1:length(local)
          )
          
          if (!is.null(tooltip)) {
            df <- cbind(df, DaparViz@metadata[tooltip])
          }
          
          colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
          if (ncol(df) > 3) {
            colnames(df)[4:ncol(df)] <- paste("tooltip_", colnames(df)[4:ncol(df)], sep = "")
          }
          
          clickFun <- JS(paste0("function(event) {Shiny.onInputChange('",
                                ns("eventPointClicked"), "', [this.index]+'_'+ [this.series.name]);}"))
          
          plotCC <- plotJitter_rCharts(df, clickFunction = clickFun)
        })
        plotCC
      })
      
      
      GetDataFor_CCMultiMulti <- reactive({
        Get_CC_Multi2Any()
        
        df <- cbind(
          id = 1:length(Get_CC_Multi2Any()),
          nProt = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {ncol(x)} )),
          nPep = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {nrow(x)})),
          proteins = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()],  function(x) {paste(colnames(x), collapse = ",")})),
          peptides = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {paste(colnames(x), collapse = ",")}))
        )
        
        colnames(df) <- c("id", "nProt", "nPep", "Proteins Ids", "Peptides Ids")
        
        df
      })
      
      output$CCMultiMulti_DL_btns_ui <- renderUI({
        req(input$searchCC == "tabular")
        dl_ui(ns("CCMultiMulti_DL_btns"))
      })
      
      # dl_server("CCMultiMulti_DL_btns",
      #                          df.data = reactive({GetDataFor_CCMultiMulti()}),
      #                          name = reactive({"CC_MultiMulti"}),
      #                          colors = reactive({NULL}),
      #                          df.tags = reactive({NULL})
      #                          )
      
      
      output$CCMultiMulti_UI <- renderUI({
        rvCC$CCMultiMulti_rows_selected <- mod_format_DT_server("CCMultiMulti", 
                                                                          data = reactive({GetDataFor_CCMultiMulti()}))
        
        
        mod_format_DT_ui(ns("CCMultiMulti"))
      })
      
      
      
      # select a CC in the summary table
      observeEvent(req(rvCC$CCMultiMulti_rows_selected()), {
        rvCC$selectedCC <- rvCC$CCMultiMulti_rows_selected()
      })
      

      observeEvent(c(rvCC$selectedNeighbors, input$node_selected, rvCC$selectedCCgraph), {
        local <- (rv$data@cc)[Get_CC_Multi2Any()]
        rvCC$selectedNeighbors
        
        nodes <- rvCC$selectedCCgraph$nodes
        
        if (!is.null(input$node_selected) && input$node_selected == 1) {
          sharedPepIndices <- intersect(rvCC$selectedNeighbors, which(nodes[, "group"] == "shared.peptide"))
          specPepIndices <- intersect(rvCC$selectedNeighbors, which(nodes[, "group"] == "spec.peptide"))
          protIndices <- intersect(rvCC$selectedNeighbors, which(nodes[, "group"] == "protein"))
        } else {
          .shared <- "shared.peptide"
          .spec <- "spec.peptide"
          sharedPepIndices <- which(nodes[, "group"] == .shared)
          specPepIndices <- which(nodes[, "group"] == .spec)
          protIndices <- which(nodes[, "group"] == "protein")
        }
        
        .shared <- nodes[sharedPepIndices, "label"]
        .specs <- nodes[specPepIndices, "label"]
        .ind <- nodes[protIndices, "label"]
        rvCC$detailedselectedNode$sharedPepLabels <- .shared
        rvCC$detailedselectedNode$specPepLabels <- .specs
        rvCC$detailedselectedNode$protLabels <- .ind
      })
      
      
      output$CCDetailed <- renderUI({
        req(rvCC$detailedselectedNode)
        req(rvCC$selectedCC)

        tagList(
          h4("Proteins"),
          uiOutput(ns("CCDetailedProt_UI")),
          h4("Specific peptides"),
          uiOutput(ns("CCDetailedSpecPep_UI")),
          h4("Shared peptides"),
          uiOutput(ns("CCDetailedSharedPep_UI"))
        )
      })
      
      # output$CCDetailedProt <- DT::renderDataTable(server = TRUE, {
      #   req(rvCC$selectedCC)
      #   rvCC$detailedselectedNode
      #   req(!is.null(rvCC$detailedselectedNode$protLabels))
      #   .protLabels <- rvCC$detailedselectedNode$protLabels
      #   
      #   df <- data.frame(proteinId = unlist(.protLabels))
      #   colnames(df) <- c("Proteins Ids")
      #   dt <- DT::datatable(df,
      #                       extensions = c("Scroller"),
      #                       options = list(
      #                         initComplete = .initComplete(),
      #                         dom = "rt",
      #                         blengthChange = FALSE,
      #                         ordering = FALSE,
      #                         scrollX = 400,
      #                         scrollY = 100,
      #                         displayLength = 10,
      #                         scroller = TRUE,
      #                         header = FALSE,
      #                         server = FALSE
      #                       )
      #   )
      #   dt
      # })
      
      
      output$CCDetailedProt_UI <- renderUI({
        req(rvCC$selectedCC)
        rvCC$detailedselectedNode
        req(!is.null(rvCC$detailedselectedNode$protLabels))
        .protLabels <- rvCC$detailedselectedNode$protLabels
        
        df <- data.frame(proteinId = unlist(.protLabels))
        colnames(df) <- c("Proteins Ids")
        
        mod_format_DT_server('CCDetailedProt', data = reactive({df}))
        mod_format_DT_ui(ns('CCDetailedProt'))
      })
      
      
      #######
      output$CCDetailedSharedPep_UI <- renderUI({
        rvCC$detailedselectedNode
        input$pepInfo
        
        req(rvCC$detailedselectedNode$sharedPepLabels)
        
        
        ind <- 1:ncol(rv$data@qdata)
        data <- FormatDataForDT(rv$data)
        .n <- ncol(data)
        pepLine <- rvCC$detailedselectedNode$sharedPepLabels
        indices <- unlist(lapply(pepLine, function(x) {which(rownames(data) == x)}))
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(input$pepInfo)) {
          data <- cbind(data, (rv$data@metadata)[pepLine, input$pepInfo])
          colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
        }
        
        offset <- length(input$pepInfo)
        c.tags <- BuildColorStyles(rv$data)$tags
        c.colors <- BuildColorStyles(rv$data)$colors
        
        hcStyle <- list(
          cols = colnames(data)[1:((.n - offset) / 2)],
          vals = colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
          unique = c.tags,
          pal = c.colors
        )
        
        mod_format_DT_server('CCDetailedSharedPep', 
                                       data = reactive({data}),
                                       hc_style = reactive({hcStyle})
                                       )
        
        mod_format_DT_ui(ns('CCDetailedSharedPep'))
        
      })
      
      
      
      # output$CCDetailedSharedPep <- DT::renderDataTable(server = TRUE, {
      #   rvCC$detailedselectedNode
      #   input$pepInfo
      #   
      #   req(rvCC$detailedselectedNode$sharedPepLabels)
      #   
      #   
      #   ind <- 1:ncol(rv$data@qdata)
      #   data <- FormatDataForDT(rv$data, rv$settings_nDigits)
      #   .n <- ncol(data)
      #   pepLine <- rvCC$detailedselectedNode$sharedPepLabels
      #   indices <- unlist(lapply(pepLine, function(x) {
      #     which(rownames(data) == x)
      #   }))
      #   data <- data[indices, c(ind, (ind + .n / 2))]
      #   
      #   if (!is.null(input$pepInfo)) {
      #     data <- cbind(data, (rv$data@metadata)[pepLine, input$pepInfo])
      #     colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
      #   }
      #   
      #   offset <- length(input$pepInfo)
      #   c.tags <- BuildColorStyles(rv$data)$tags
      #   c.colors <- BuildColorStyles(rv$data)$colors
      #   
      #   dt <- DT::datatable(data,
      #                       extensions = c("Scroller"),
      #                       options = list(
      #                         initComplete = initComplete(),
      #                         dom = "rt",
      #                         blengthChange = FALSE,
      #                         ordering = FALSE,
      #                         scrollX = 400,
      #                         scrollY = 150,
      #                         displayLength = 10,
      #                         scroller = TRUE,
      #                         header = FALSE,
      #                         server = FALSE,
      #                         columnDefs = list(
      #                           list(
      #                             targets = c(
      #                               (((.n - offset) / 2) + 1):(.n - offset)
      #                             ),
      #                             visible = FALSE
      #                           )
      #                         )
      #                       )
      #   ) %>%
      #     DT::formatStyle(
      #       colnames(data)[1:((.n - offset) / 2)],
      #       colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
      #       backgroundColor = DT::styleEqual(c.tags, c.colors)
      #     )
      #   
      #   dt
      # })
      
      
      
      output$CCDetailedSpecPep_UI <- renderUI({
        rvCC$detailedselectedNode
        input$pepInfo
        req(rvCC$detailedselectedNode$specPepLabels)
        
        ind <- 1:ncol(rv$data@qdata)
        data <- FormatDataForDT(rv$data)
        .n <- ncol(data)
        pepLine <- rvCC$detailedselectedNode$specPepLabels
        indices <- unlist(lapply(pepLine, function(x) {which(rownames(data) == x)}))
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(input$pepInfo)) {
          data <- cbind(data, (rv$data@metadata)[pepLine, input$pepInfo])
          colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
        }
        
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(rv$data)$tags
        c.colors <- BuildColorStyles(rv$data)$colors
        
        hcStyle <- list(
          cols = colnames(data)[1:((.n - offset) / 2)],
          vals = colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
          unique = c.tags,
          pal = c.colors
        )
        
        mod_format_DT_server('CCDetailedSpecPep', 
                                       data = reactive({data}),
                                       hc_style = reactive({hcStyle})
                                       )
        
        mod_format_DT_ui(ns('CCDetailedSpecPep'))
        
      })
      
      
       
      ##### -----------
      # output$CCDetailedSpecPep <- DT::renderDataTable(server = TRUE, {
      #   rvCC$detailedselectedNode
      #   input$pepInfo
      #   req(rvCC$detailedselectedNode$specPepLabels)
      #   
      #   ind <- 1:ncol(rv$data@qdata)
      #   data <- FormatDataForDT(rv$data, rv$settings_nDigits)
      #   .n <- ncol(data)
      #   pepLine <- rvCC$detailedselectedNode$specPepLabels
      #   indices <- unlist(lapply(pepLine, function(x) {which(rownames(data) == x)}))
      #   data <- data[indices, c(ind, (ind + .n / 2))]
      #   
      #   if (!is.null(input$pepInfo)) {
      #     data <- cbind(data, (rv$data@metadata)[pepLine, input$pepInfo])
      #     colnames(data)[(1 + .n - length(input$pepInfo)):.n] <- input$pepInfo
      #   }
      #   
      #   offset <- length(input$pepInfo)
      #   
      #   c.tags <- BuildColorStyles(rv$data)$tags
      #   c.colors <- BuildColorStyles(rv$data)$colors
      #   
      #   dt <- DT::datatable(data,
      #                       extensions = c("Scroller"),
      #                       options = list(
      #                         initComplete = initComplete(),
      #                         dom = "rt",
      #                         blengthChange = FALSE,
      #                         ordering = FALSE,
      #                         scrollX = 400,
      #                         scrollY = 100,
      #                         displayLength = 10,
      #                         scroller = TRUE,
      #                         header = FALSE,
      #                         server = FALSE,
      #                         columnDefs = list(
      #                           list(
      #                             targets = c(
      #                               (((.n - offset) / 2) + 1):(.n - offset)
      #                             ),
      #                             visible = FALSE
      #                           )
      #                         )
      #                       )
      #   ) %>%
      #     DT::formatStyle(
      #       colnames(data)[1:((.n - offset) / 2)],
      #       colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
      #       backgroundColor = DT::styleEqual(c.tags, c.colors)
      #     )
      #   
      #   dt
      # })
      
      
      
      
      
      
      Get_CC_One2One <- reactive({
        #rv$data@cc
        ll.prot <- lapply(rv$data@cc, function(x) {ncol(x)})
        ll.pept <- lapply(rv$data@cc, function(x) {nrow(x)})
        ll.prot.one2one <- intersect(which(ll.prot == 1), which(ll.pept == 1))
        ll.prot.one2one
      })
      
      
      
      Get_CC_One2multi <- reactive({
        #rv$data@cc
        ll.prot <- lapply(rv$data@cc, function(x) {ncol(x)})
        ll.pept <- lapply(rv$data@cc, function(x) {nrow(x)})
        ll.prot.one2multi <- intersect(which(ll.prot == 1),  which(ll.pept > 1))
        ll.prot.one2multi
      })
      
      
      Get_CC_Multi2Any <- reactive({
        #rv$data@cc
        ll.prot <- lapply(rv$data@cc, function(x) {ncol(x)})
        ll.pept <- lapply(rv$data@cc, function(x) {nrow(x)})
        ll.prot.multi2any <- which(ll.prot > 1)
        ll.prot.multi2any
      })
      
      
      
      BuildOne2OneTab <- reactive({
        #rv$data@cc
        df <- cbind(
          cbind(lapply((rv$data@cc)[Get_CC_One2One()], function(x) {colnames(x)})),
          cbind(lapply((rv$data@cc)[Get_CC_One2One()], function(x) {rownames(x)}))
        )
        colnames(df) <- c("proteins", "peptides")
        df
      })
      
      BuildOne2MultiTab <- reactive({
        #rv$data@cc
        
        df <- cbind(
          proteins = cbind(lapply((rv$data@cc)[Get_CC_One2multi()], function(x) {colnames(x)})),
          nPep = cbind(lapply((rv$data@cc)[Get_CC_One2multi()], function(x) {nrow(x)})),
          peptides = cbind(lapply((rv$data@cc)[Get_CC_One2multi()],function(x) {paste(rownames(x), collapse = ",")}))
        )
        colnames(df) <- c("proteins", "nPep", "peptides")
        
        df
      })
      
      
      BuildMulti2AnyTab <- reactive({
        #rv$data@cc
        df <- cbind(id = 1:length(Get_CC_Multi2Any()),
          proteins = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {colnames(x)})),
          nProt = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {ncol(x)})),
          nPep = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {nrow(x)})),
          peptides = cbind(lapply((rv$data@cc)[Get_CC_Multi2Any()], function(x) {paste(rownames(x), collapse = ",")}))
        )
        colnames(df) <- c("proteins", "nPep", "peptides")
        
        df
      })
      
      
      
      # dl_server("OneMultiDT_DL_btns",
      #                          df.data = reactive({
      #                            df <- BuildOne2MultiTab()
      #                            colnames(df) <- c("Proteins Ids", "nPep", "Peptides Ids")
      #                            df
      #                          }),
      #                          name = reactive({"CC_OneMulti"}),
      #                          colors = reactive({NULL}),
      #                          df.tags = reactive({NULL})
      #                          )
      
      
      output$OneMultiDT_UI <- renderUI({
        req(rv$data@cc)
        df <- BuildOne2MultiTab()
        colnames(df) <- c("Proteins Ids", "nPep", "Peptides Ids")
        
        
        rvCC$OneMultiDT_rows_selected <- mod_format_DT_server('OneMultiDT', 
                                                                        data = reactive({df}))
        mod_format_DT_ui(ns('OneMultiDT'))
      })
      

      GetDataFor_OneMultiDTDetailed <- reactive({
        input$pepInfo
        req(rvCC$OneMultiDT_rows_selected())
        
        line <- rvCC$OneMultiDT_rows_selected()
        ind <- 1:ncol(rv$data@qdata)
        data <- FormatDataForDT(rv$data, 2)
        .n <- ncol(data)
        .pep <- input$pepInfo
        pepLine <- unlist(strsplit(unlist(BuildOne2MultiTab()[line, "peptides"]), split = ","))
        
        indices <- unlist(lapply(pepLine, function(x) {which(rownames(data) == x)}))
        
        data <- data[indices, c(ind, (ind + .n / 2))]
        
        if (!is.null(.pep)) {
          data <- cbind(data, (rv$data@metadata)[pepLine, .pep])
          colnames(data)[(1 + .n - length(.pep)):.n] <- .pep
        }
        
        data
      })
      
      
      output$OneMultiDTDetailed_UI <- renderUI({
        input$pepInfo
        req(rvCC$OneMultiDT_rows_selected())
        
        data <- GetDataFor_OneMultiDTDetailed()
        .n <- ncol(data)
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(rv$data)$tags
        c.colors <- BuildColorStyles(rv$data)$colors
        
        hcStyle <- list(
          cols = colnames(data)[1:((.n - offset) / 2)],
          vals = colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
          unique = c.tags,
          pal = c.colors
        )
        
        mod_format_DT_server('OneMultiDTDetailed', 
                                       data = reactive({data}),
                                       hc_style = reactive({hcStyle})
                                       )
        mod_format_DT_ui(ns('OneMultiDTDetailed'))
      })
      
      
      
      
      # dl_server("OneOneDT_DL_btns",
      #                          df.data = reactive({
      #                            df <- BuildOne2OneTab()
      #                            colnames(df) <- c("Proteins Ids", "Peptides Ids")
      #                            df
      #                          }),
      #                          name = reactive({"CC_OneOne"}),
      #                          colors = reactive({NULL}),
      #                          df.tags = reactive({NULL})
      #                          )
      
      
      output$OneOneDT_UI <- renderUI({
        req(rv$data@cc)
        df <- BuildOne2OneTab()
        colnames(df) <- c("Proteins Ids", "Peptides Ids")
        rvCC$OneOneDT_rows_selected <- mod_format_DT_server('OneOneDT', 
                                                                      data = reactive({df}),
                                                                      hc_style = reactive({NULL})
                                                                      )
        
        mod_format_DT_ui(ns('OneOneDT'))
      })
      
      
      
      GetDataFor_OneOneDTDetailed <- reactive({
        req(rv$data@cc)
        req(rvCC$OneOneDT_rows_selected())
        input$pepInfo
        .pep <- input$pepInfo
        line <- rvCC$OneOneDT_rows_selected()
        ind <- 1:ncol(rv$data@qdata)
        data <- FormatDataForDT(rv$data)
        .n <- ncol(data)
        
        pepLine <- BuildOne2OneTab()[line, 2]
        
        indices <- unlist(lapply(pepLine, function(x) {
          which(rownames(data) == x)
        }))
        data <- data[indices, c(ind, (ind + .n / 2))]
        if (!is.null(.pep)) {
          data <- cbind(data, (rv$data@metadata)[pepLine, .pep])
          colnames(data)[(1 + .n - length(.pep)):.n] <- .pep
        }
        
        data
      })
      
      
      output$OneOneDTDetailed_UI <- renderUI({
        req(rv$data@cc)
        req(rvCC$OneOneDT_rows_selected())
        data <- GetDataFor_OneOneDTDetailed()
        .n <- ncol(data)
        offset <- length(input$pepInfo)
        
        c.tags <- BuildColorStyles(rv$data)$tags
        c.colors <- BuildColorStyles(rv$data)$colors
        
        hcStyle <- list(
          cols = colnames(data)[1:((.n - offset) / 2)],
          vals = colnames(data)[(((.n - offset) / 2) + 1):(.n - offset)],
          unique = c.tags,
          pal = c.colors
        )
        
        mod_format_DT_server('OneOneDTDetailed', 
                                       data = reactive({data}),
                                       hc_style = reactive({hcStyle})
                                       )
        
        mod_format_DT_ui(ns('OneOneDTDetailed'))
      })
      
      
    }
  )
}
