
#' @title Compare two assays
#'
#' @description
#' This plot compares the quantitative proteomics data between two assays.
#' It can be used for example to compare the effect of the normalization 
#' process.
#'
#' The comparison is made with the division operator.
#'
#' @param assay1 A numeric matrix containing quantitative data before 
#' normalization.
#'
#' @param assay2 A numeric matrix containing quantitative data after 
#' normalization.
#'
#' @param conds A vector of the conditions (one condition per sample).
#'
#' @param info xxx
#'
#' @param pal.name xxx
#'
#' @param subset.view xxx
#'
#' @param n xxx
#'
#' @param type scatter or line
#'
#' @param FUN xxx
#'
#' @return A plot
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @examples
#' library(SummarizedExperiment)
#' data(ft, package='DaparViz')
#' assay1 <- assay(ft, 1)
#' assay2 <- 3 * assay(ft, 1)
#' conds <- colData(ft)$Condition
#'
#' ## -----------------------------------------------------------------
#' ##
#' ## -----------------------------------------------------------------
#'
#' plotCompareAssays(assay1, assay2, conds, n = 5)
#'
#' @import highcharter
#' @importFrom tibble as_tibble
#' @importFrom utils str
#'
#' @export
#'
#' @rdname compare-assays
#'
plotCompareAssays <- function(assay1,
    assay2,
    conds,
    info = NULL,
    pal.name = NULL,
    subset.view = NULL,
    n = 100,
    type = "scatter",
    FUN = NULL) {
    if (missing(assay1)) {
        stop("'assay1' is missing")
    }
    stopifnot(inherits(assay1, "matrix"))

    if (missing(assay2)) {
        stop("'assay2' is missing")
    }
    stopifnot(inherits(assay2, "matrix"))

    if (missing(conds)) {
        stop("'conds' is missing")
    }

    if (!all.equal(dim(assay1), dim(assay2))) {
        stop("Assays must have the same dimensions.")
    }

    if (ncol(assay1) != length(conds)) {
        stop("The length of 'conds' must be equal to the number of columns of
    'assay1' and 'assay2'")
    }


    if (is.null(info)) {
        info <- rep(NA, nrow(assay1))
    } else if (length(info) != nrow(assay1)) {
        stop("'info' must have the same length as the number of rows of
         'assay1' and 'assay2'")
    }


    stopifnot(type %in% c("scatter", "line"))

    # Reduce the assays to the entities given by 'subset.view'
    if (!is.null(subset.view) && length(subset.view) > 0) {
        if (nrow(assay1) > 1) {
            info <- info[subset.view]

            if (length(subset.view) == 1) {
                assay1 <- t(assay1[subset.view, ])
                assay2 <- t(assay2[subset.view, ])
            } else {
                assay1 <- assay1[subset.view, ]
                assay2 <- assay2[subset.view, ]
            }
        }
    }


    if (is.null(n) || n > nrow(assay1)) {
        warning("'n' is NULL or higher than the number of rows of datasets.
      Set to number of rows.")
        n <- nrow(assay1)
    }



    # Reduce the assays to 'n' entities so as to make a lighter plot
    if (nrow(assay1) > 1) {
        ind <- sample(seq_len(nrow(assay1)), n)
        info <- info[ind]
        if (length(ind) == 1) {
            assay1 <- t(assay1[ind, ])
            assay2 <- t(assay2[ind, ])
        } else {
            assay1 <- assay1[ind, ]
            assay2 <- assay2[ind, ]
        }
    }


    myColors <- ExtendPalette(length(unique(conds)), pal.name)

    # Compare by using the division between assays
    x <- assay1
    y <- assay2 / assay1

    series <- list()
    for (i in seq_len(length(conds))) {
        series[[i]] <- list(
            name = colnames(x)[i],
            data = highcharter::list_parse(
                data.frame(
                    x = x[, i],
                    y = y[, i],
                    name = info
                )
            )
        )
    }

    h1 <- highchart() %>%
        customChart(chartType = type) %>%
        hc_add_series_list(series) %>%
        hc_colors(myColors) %>%
        customExportMenu(fname = "compareAssays")

    if (!all.equal(info, rep(NA, length(info)))) {
        h1 <- h1 %>%
            hc_tooltip(headerFormat = "", pointFormat = "Id: {point.name}")
    } else {
        h1 <- h1 %>%
            hc_tooltip(enabled = FALSE)
    }


    h1
}
