#' @title Explore a `VizClass` object
#'
#' @description
#' 
#' @param id A `character(1)` which is the id of the shiny module.
#' @param vizData An instance of the class `VizData`
#' @param digits xxx
#'
#'
#' @name SE-explorer
#'
#' @example examples/example_mod_ds_seExplorer.R
#' 
NULL


#' @export
#' @import shiny
#' @rdname SE-explorer
#' @import shinyBS
#' 
mod_ds_seExplorer_ui <- function(id) {
    ns <- NS(id)
    tagList(
      mod_colorLegend_ui(ns("legend")),
      shinyBS::bsCollapse(id = "infos", open = "", multiple = TRUE,
            shinyBS::bsCollapsePanel("Quantitative data",
                DT::DTOutput(ns("qdata_ui")),
                style = "info"),
            shinyBS::bsCollapsePanel("Metadata",
                DT::DTOutput(ns("metadata_ui")),
                style = "info"),
            shinyBS::bsCollapsePanel("quantitative Metadata",
                DT::DTOutput(ns("qMetacell_ui")),
                style = "info")
            )
      )
}

#'
#' @export
#'
#' @return NA
#' @import DT
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#'
#' @rdname SE-explorer
mod_ds_seExplorer_server <- function(id,
                                     vizData = reactive({NULL}),
                                     digits = reactive({3})) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observe({
          req(vizData())
          stopifnot(inherits(vizData(), "VizData"))
            tags <- GetMetacellTags(vizData()@metacell, 
                                    level = vizData()@type, 
                                    onlyPresent = TRUE)
            mod_colorLegend_server("legend", tags)
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
        #       extensions = c('Scroller', 'Buttons'),
        #       rownames=  FALSE,
        #       options=list(initComplete = .initComplete(),
        #       dom = 'Brtip',
        #       pageLength=10,
        #       orderClasses = TRUE,
        #       autoWidth=TRUE,
        #       deferRender = TRUE,
        #       bLengthChange = FALSE,
        #       scrollX = 200,
        #       scrollY = 500,
        #       scroller = TRUE,
        #       columnDefs = list(list(width='60px',targets= "_all"))
        #       )) %>%
        #         DT::formatStyle(
        #           columns = colnames(data)[seq_len(2)],
        #           valueColumns = colnames(data)[2],
        #           backgroundColor = DT::styleEqual(
        #           unique(data$Condition),
        #           pal[seq_len(length(unique(data$Condition)))])
        #         )
        #
        #     })


        output$metadata_ui <- DT::renderDT({
          
            req(vizData())

            dat <- DT::datatable(tibble::as_tibble(vizData()@metadata),
                rownames = TRUE,
                extensions = c("Scroller", "Buttons", "FixedColumns"),
                options = list(
                    initComplete = .initComplete(),
                    dom = "Bfrtip",
                    pageLength = 10,
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
                            targets = c(list(0), list(1), list(2))
                        )
                    )
                )
            )

            if ("Significant" %in% colnames(vizData()@metadata)) {
                dat <- dat %>%
                    DT::formatStyle(
                        columns = "Significant",
                        target = "row",
                        background = DT::styleEqual(1, "lightblue")
                    )
            }

            return(dat)
        })


        output$qdata_ui <- DT::renderDataTable(server = TRUE, {
            req(vizData())
            
           df <- cbind(
                keyId = (vizData()@metadata)[, vizData()@colID],
                round(vizData()@qdata, digits = digits()),
                vizData()@metacell)

            colors <- custom_metacell_colors()

            DT::datatable(df,
                extensions = c("Scroller"),
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
                            targets = c(((2 + (ncol(df) - 1) / 2)):ncol(df)),
                            visible = FALSE
                        )
                    )
                )
            ) %>%
                DT::formatStyle(
                    colnames(df)[2:(1 + (ncol(df) - 1) / 2)],
                    colnames(df)[((2 + (ncol(df) - 1) / 2)):ncol(df)],
                    backgroundColor = DT::styleEqual(
                        names(colors),
                        unname(unlist(colors))
                    ),
                    backgroundSize = "98% 48%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })

        output$qMetacell_ui <- DT::renderDataTable(server = TRUE, {
            req(vizData())
          df <- vizData()@metacell
            colors <- custom_metacell_colors()

            DT::datatable(df,
                extensions = c("Scroller"),
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
                    backgroundColor = DT::styleEqual(
                        names(colors),
                        unname(unlist(colors))
                    ),
                    backgroundSize = "98% 48%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })
    })
}

