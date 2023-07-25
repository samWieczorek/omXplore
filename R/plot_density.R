#' @param data A `data.frame` or `matrix` of `numeric()` which is the 
#' quantitative data to plot.
#'
#' @param conds A `character()` of condition name for each sample. The
#' length of 'conds' must be equal to the number of columns of 'data'.
#'
#' @param pal.name A `character(1)` which is the name of the palette from 
#' the package [RColorBrewer] from which the colors are taken. Default 
#' value is 'Set1'.
#'
#' @import highcharter
#' @importFrom stats density
#'
#' @export
#' 
#' @example examples/ex_mod_ds_density.R
#'
#' @rdname density-plot
#' 
#' @return NA
#'
densityPlot <- function(data,
                        conds,
                        pal.name = NULL) {
    if (missing(data)) {
        stop("'data' is missing.")
    }

    if (missing(conds)) {
        stop("'conds' is missing.")
    }

    if (length(conds) != ncol(data)) {
        stop("data and conds must have the same number of samples.")
    }

    myColors <- SampleColors(conds, pal.name)


    h1 <- highcharter::highchart() %>%
        hc_title(text = "Density plot") %>%
        customChart(chartType = "spline", zoomType = "x") %>%
        hc_legend(enabled = TRUE) %>%
        hc_xAxis(title = list(text = "log(Intensity)")) %>%
        hc_yAxis(title = list(text = "Density")) %>%
        hc_tooltip(
            headerFormat = "",
            pointFormat = "<b> {series.name} </b>: {point.y} ",
            valueDecimals = 2
        ) %>%
        customExportMenu(fname = "densityplot") %>%
        hc_plotOptions(
            series = list(
                animation = list(
                    duration = 100
                ),
                connectNulls = TRUE,
                marker = list(
                    enabled = FALSE
                )
            )
        ) %>%
        hc_colors(myColors)



    for (i in seq_len(ncol(data))) {
        tmp <- data.frame(
            x = stats::density(data[, i], na.rm = TRUE)$x,
            y = stats::density(data[, i], na.rm = TRUE)$y
        )

        h1 <- h1 %>%
            hc_add_series(
                data = list_parse(tmp),
                name = colnames(data)[i]
            )
    }

    h1
}
