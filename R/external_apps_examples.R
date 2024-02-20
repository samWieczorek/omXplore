#' @title Displays a correlation matrix of the quantitative data of a
#' numeric matrix.
#'
#' @description
#' xxxx
#'
#' @name external_app
#'
#' @param id A `character(1)` which is the id of the shiny module.
#' @param obj An instance of the class `DaparViz`
#'
#' @return A shiny app
#'
#' @examples
#' if (interactive()) {
#'   data(vData_ft)
#'   extFoo1(vData_ft[[1]])
#' }
#'
NULL

#' @importFrom shiny NS tagList
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
    plotOutput(ns("plot"))
  )
}

#' @rdname external_app
#' @export
#' @return NA
#'
extFoo1_server <- function(
    id,
    obj = reactive({
      NULL
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe(
      {
        req(obj())
        if (inherits(obj(), "DaparViz")) {
          rv$data <- obj()
        } else {
          shinyjs::toggle("badFormatMsg", condition = !inherits(obj(), "list"))
        }
      },
      priority = 1000
    )

    output$plot <- renderPlot({
      req(rv$data)
      plot(rv$data@qdata)
    })
  })
}




#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo1 <- function(obj) {
  ui <- extFoo1_ui("plot")

  server <- function(input, output, session) {
    extFoo1_server("plot", reactive({
      obj
    }))
  }

  shinyApp(ui = ui, server = server)
}




#' @importFrom shiny NS tagList
#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(div(id = ns("badFormatMsg"), h3(bad_format_txt))),
    plotOutput(ns("plot"))
  )
}

#' @rdname external_app
#' @export
#' @return NA
#'
extFoo2_server <- function(
    id,
    obj = reactive({
      NULL
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data = NULL
    )

    observe(
      {
        req(obj())
        if (inherits(obj(), "DaparViz")) {
          rv$data <- obj()
        } else {
          shinyjs::toggle("badFormatMsg", condition = !inherits(obj(), "list"))
        }
      },
      priority = 1000
    )

    output$plot <- renderPlot({
      req(rv$data)
      plot(rv$data@qdata)
    })
  })
}



#' @export
#' @rdname external_app
#' @return A shiny app
#'
extFoo2 <- function(obj) {
  ui <- extFoo2_ui("plot")

  server <- function(input, output, session) {
    extFoo2_server("plot", reactive({
      obj
    }))
  }

  shinyApp(ui = ui, server = server)
}
