#' @title Bar plot of missing values per lines using `highcharter`.
#'
#' @description
#'
#' This method plots a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (i.e. proteins).
#'
#'
#' @details
#'
#' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (i.e. proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#'
#' - Variance : Builds a densityplot of the CV of entities in numeric matrix.
#' The CV is calculated for each condition present in the dataset
#' (see the slot \code{'Condition'} in the \code{colData()} DataFrame)
#'
#' - Heatmap:
#'
#'
#' The function [heatmapD()]
#'
#'
#' The function [] is inspired from the function 'heatmap.2'
#' that displays a numeric matrix. For more information, please refer to the
#' help of the heatmap.2 function.
#'
#'
#' @section Missing values:
#'
#' #' - distribution of the missing values per line,
#'
#' - a bar plot which represents the distribution of the
#' number of missing values (NA) per lines (ie proteins) and per conditions,
#'
#' - Histogram of missing values.
#'
#' @name ds-view
#'
#' @param id A `character(1)` for the 'id' of the shiny module. It must be
#' the same as for the '*_ui' function.
#' @param obj A list of items of the class `DaparViz`.
#' @param addons A `list` to configure the other shiny apps to integrate.
#' Each item correspond to one package:
#' * the name of the slot is the name of the package
#' * the content of the slot is a vector composed of the generic name of the
#' shiny app. Each of the apps listed here must be an exported app of the
#' package.
#' For example, given the value addons = list(testPkg = c('foo', 'foo2')). That
#' means that the package called "testPkg" must provide the four functions:
#' foo1_ui(), foo1_server() and foo2_ui(), foo2_server())
#' @param width xxx
#' @param height xxx
#'
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' if (interactive()) {
#'   data(vData_ft)
#'  # addon <- list(DaparToolshed = c("DaparViz_metacell"))
#'   addons <- list(DaparViz = list(funcs = c("extFoo1"), imgDirPath = ''))
#'   view_dataset(vData_ft, addons)
#' }
#'
NULL




#' @import shiny
#' @import shinyBS
#' @importFrom shinyjs useShinyjs
#' @rdname ds-view
#' @export
#' @return NA
#'
view_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidPage(
      shinyjs::hidden(
        div(id = ns("badFormatMsg"), p(bad_format_txt))
      ),
      fluidRow(
        column(3, div(
          style = general_style,
          uiOutput(ns("chooseDataset_ui"))
        )),
        column(9, div(
          style = general_style,
          uiOutput(ns("ShowPlots_ui"))
        ))
      )
      # br(), br(), br(),
      # uiOutput(ns("ShowPlots_ui"))
    )
  )
}

#'
#' @importFrom shinyjs show hide hidden
#' @import shiny
#' @import shinyBS
#'
#' @rdname ds-view
#' @export
#' @return NA
#'
view_dataset_server <- function(
    id,
    obj = NULL,
    addons = list(),
    width = 40,
    height = 40) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL,
      conds = NULL,
      current.se = NULL,
      btns.history = NULL,
      ll.mods = NULL
    )

    observe(
      {
        req(obj())
        if (inherits(obj(), "list")) {
          rv$data <- obj()
          conds <- rv$data[[1]]@conds

          addModules(addons)
          rv$ll.mods <- listPlotModules()
          # print(rv$ll.mods)
        } else {
          shinyjs::toggle("badFormatMsg", condition = !inherits(obj(), "list"))
        }
      },
      priority = 1000
    )


    output$ShowPlots_ui <- renderUI({
      req(c(rv$data, rv$ll.mods))
      lapply(rv$ll.mods, function(x) {
        tagList(
          actionButton(
            ns(x),
            label = tagList(
              p(gsub("DaparViz_", "", x)),
              tags$img(src = FindImgSrc(x), height = "50px")
            ),
            style = "padding: 5px; border: none;
              background-size: cover; background-position: center;"
          ),
          shinyBS::bsModal(ns(paste0("window_", x)),
            title = x,
            trigger = ns(x),
            footer = NULL,
            do.call(
              paste0(x, "_ui"),
              list(id = ns(paste0(x, "_large")))
            )
            
            # Here, we could put the global function that calls shinyApp with
            # the module but it takes a longer time to display than if the
            # server is laready launched elsewhere
            #do.call(x, list(obj = rv$current.se))
          )
        )
      })
    })


    observe({
      req(rv$current.se)
      req(rv$ll.mods)

      for (x in rv$ll.mods) {
        # print(paste0('Launch : ', x))
        do.call(
          paste0(x, "_server"),
          list(
            id = paste0(x, "_large"),
            obj = reactive({
              rv$current.se
            })
          )
        )
      }
    })

    FindImgSrc <- function(x) {
      # By default, search image from the images directory of the DaparViz
      # package. This works for built-in plot modules. For external modules,
      # then load customized resource path


      img_path <- system.file("images", paste0(x, ".png"), package = "DaparViz")
      if (file.exists(img_path)) {
        img_src <- paste0("images/", x, ".png")
      } else {
        img_src <- paste0("DaparViz_imgDir/", gsub("DaparViz_", "", x), ".png")
      }

      img_src
    }


    # Update current.se variable
    observeEvent(req(rv$data, input$chooseDataset), ignoreNULL = TRUE, {
      rv$current.se <- rv$data[[input$chooseDataset]]
    })

    output$chooseDataset_ui <- renderUI({
      req(rv$data)

      if (length(rv$data) == 0) {
        choices <- list(" " = character(0))
      } else {
        choices <- names(rv$data)
      }

      radioButtons(ns("chooseDataset"), "Dataset",
        choices = choices,
        selected = names(rv$data)[length(rv$data)],
        width = 200
      )
    })
  })
}


#' @export
#' @rdname ds-view
#' @import shiny
#' @import shinyBS
#'
#' @return A shiny application which wraps the functions view_dataset_ui()
#' and the view_dataset_server()
#'
#' @examples
#' if (interactive()) {
#'   data(vData_ft)
#'   view_dataset(vData_ft)
#' }
#'
view_dataset <- function(
    obj = NULL,
    addons = NULL) {
  ui <- fluidPage(
    view_dataset_ui("dataset")
  )

  server <- function(input, output, session) {
    view_dataset_server("dataset",
      obj = reactive({
        obj
      }),
      addons = addons
    )
  }

  shinyApp(ui, server)
}
