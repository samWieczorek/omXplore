
#' @param se A instance of the class `SummarizedExperiment`
#' @param conds A `character()` of the name of conditions (one condition per 
#' sample).
#'
#' @param pal.name A `character(1)` which is the name of the palette (from
#' the package [RColorBrewer] to use.
#'
#' @param pattern xxx
#'
#' @param typeofMV xxx
#'
#' @param title The title of the plot
#' @export
#'
#' @rdname plot-mv
#'
mv.density <- function(se,
    conds,
    pal.name = NULL,
    pattern,
    typeofMV = NULL,
    title = NULL) {
    stopifnot(inherits(se, "SummarizedExperiment"))

    if (!requireNamespace("DaparToolshed", quietly = TRUE)) {
        stop("Please install DaparToolshed: 
            BiocManager::install('DaparToolshed')")
    }
    qData <- SummarizedExperiment::assay(se)
    uconds <- unique(conds)
    palette <- SampleColors(uconds, pal.name)
    mTemp <- nbNA <- nbValues <- matrix(rep(0, nrow(qData) * length(uconds)),
        nrow = nrow(qData),
        dimnames = list(NULL, uconds)
    )


    dataCond <- data.frame()
    ymax <- 0
    series <- list()
    myColors <- NULL
    j <- 1

    for (iCond in uconds) {
        ind.conds <- which(conds == iCond)
        tags <- match.qMetadata(
            qMetadata(se)[, ind.conds],
            pattern = pattern,
            level = typeDataset(se)
        )
        for (l in seq(length(ind.conds))) {
            ind <- which(rowSums(tags) == l)
            tmp <- NULL
            if (length(ind) > 0) {
                dat <- rowMeans(qData[ind, ind.conds], na.rm = TRUE)
                tmp <- density(dat, na.rm = TRUE)
                tmp$y <- tmp$y + l
            }
            series[[j]] <- tmp
            myColors <- c(myColors, palette[which(uconds == iCond)])
            j <- j + 1
        }
    }

    axis.title <- list(
        x = "Mean of intensities",
        y = paste0("Number of '", pattern, "' per condition")
    )

    hc <- highchart(type = "chart") %>%
        hc_title(text = title) %>%
        customChart(chartType = "spline", zoomType = "xy") %>%
        hc_legend(
            align = "left", verticalAlign = "top",
            layout = "vertical"
        ) %>%
        hc_xAxis(title = list(text = axis.title$x)) %>%
        hc_yAxis(
            title = list(text = axis.title$y),
            tickInterval = 1
        ) %>%
        hc_tooltip(
            headerFormat = "",
            pointFormat = "<b> {series.name} </b>: {point.y} ",
            valueDecimals = 2
        ) %>%
        customExportMenu(fname = paste0(pattern, "_distribution")) %>%
        hc_plotOptions(
            series = list(
                showInLegend = TRUE,
                animation = list(
                    duration = 100
                ),
                connectNulls = TRUE,
                marker = list(
                    enabled = FALSE
                )
            )
        )

    for (i in seq(length(series))) {
        hc <- hc_add_series(hc,
            data = list_parse(data.frame(cbind(
                x = series[[i]]$x,
                y = series[[i]]$y
            ))),
            showInLegend = FALSE,
            color = myColors[i],
            name = conds[i]
        )
    }

    # add three empty series for the legend entries. Change color and 
    # marker symbol
    for (c in seq(length(uconds))) {
        hc <- hc_add_series(hc,
            data = data.frame(),
            name = uconds[c],
            color = palette[c],
            marker = list(symbol = "circle"),
            type = "line"
        )
    }

    hc
    return(hc)
}







#' @param se An instance of a class `SummarizedExperiment`.
#' @param conds A `character()` of the name of conditions (one condition 
#' per sample).
#'
#' @rdname plot-mv
#' @export
#' @importFrom stats setNames

mv.mec.heatmap <- function(se, conds) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Please install SummarizedExperiment: 
            BiocManager::install('SummarizedExperiment')")
    }

    if (missing(se)) {
        stop("'se' is required.")
    }
    stopifnot(inherits(se, "SummarizedExperiment"))

    if (missing(conds)) {
        stop("'conds' is required.")
    }

    stopifnot("qMetadata" %in% colnames(SummarizedExperiment::rowData(se)))
    q.tmp <- SummarizedExperiment::rowData(se)[["qMetadata"]]
    i_MEC <- which(apply(q.tmp == "missing MEC", 1, sum) > 0)

    if (length(i_MEC) == 0) {
        warning("The dataset contains no Missing value on Entire 
        Condition (MEC). So this plot is not available.")
        return(NULL)
    } else if (length(i_MEC) == 1) {
        warning("The dataset contains only one Missing value on Entire 
        Condition (MEC). Currently, Prostar does not handle such dataset to 
        build the plot. As it has no side-effects on the results, you can 
        continue your imputation.")
        return(NULL)
    }

    data <- SummarizedExperiment::assay(se)[i_MEC, ]


    ### build indices of conditions
    indCond <- list()
    conds.names <- unique(conds)

    indCond <- setNames(
        lapply(
            conds.names,
            function(x) {
                grep(x, conds)
            }
        ),
        paste0("cond", seq_len(length(conds.names)))
    )

    nNA1 <- apply(
        as.matrix(data[, indCond$cond1]),
        1,
        function(x) {
            sum(is.na(x))
        }
    )
    nNA2 <- apply(
        as.matrix(data[, indCond$cond2]),
        1,
        function(x) {
            sum(is.na(x))
        }
    )

    o <- order(((nNA1 + 1)^2) / (nNA2 + 1))
    exprso <- data[o, ]

    for (i in seq(nrow(exprso))) {
        k <- order(exprso[i, indCond$cond1])
        exprso[i, rev(indCond$cond1)] <- exprso[i, k]
        .temp <- mean(exprso[i, rev(indCond$cond1)], na.rm = TRUE)
        exprso[i, which(!is.na(exprso[i, indCond$cond1]))] <- .temp

        k <- order(exprso[i, indCond$cond2])
        exprso[i, indCond$cond2] <- exprso[i, k + length(indCond$cond1)]
        .temp <- mean(exprso[i, indCond$cond2], na.rm = TRUE)
        exprso[i, length(indCond$cond1) +
            which(!is.na(exprso[i, indCond$cond2]))] <- .temp
    }

    colors <- colorRampPalette(c("yellow", "red"))(100)
    mv.heatmap(
        x = exprso,
        col = colors,
        key = TRUE,
        srtCol = 0,
        labCol = conds,
        ylab = "Peptides / proteins",
        main = "MEC heatmap"
    )

    # heatmap_HC(exprso,col = colfunc(100),labCol=conds)
}
