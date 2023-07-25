
#' @param data An instance of the class `VizData`.
#' @param conds A `character()` of the name of conditions
#' (one condition per sample). The number of conditions must be equal to
#' the number of samples (number of columns) of the parameter 'data'.
#' @param var.scaling The dimensions to plot
#' @param ncp A `integer(1)` which represents the umber of dimensions kept in 
#' the results.
#'
#' @importFrom stats na.omit
#' 
#' @example examples/ex_mod_ds_pca.R
#'
#' @export
#'
#' @rdname ds-pca
#'
wrapper_pca <- function(vizData,
                        var.scaling = TRUE,
                        ncp = NULL) {
    if (!requireNamespace("FactoMineR", quietly = TRUE)) {
        stop("Package \"FactoMineR\" must be installed to use this function.", call. = FALSE)
    }

    if (missing(vizData)) {
        stop("'vizData' is missing.")
    }

    
    if (is.null(var.scaling)) {
        var.scaling <- TRUE
    }

  res.pca <- NULL
 
  if (length(which(is.na(vizData@qdata))) > 0) {
    if (is.null(ncp)) {
        nmax <- 12
        y <- vizData@qdata
        nprot <- dim(y)[1]
        n <- dim(y)[2] # If too big, take the number of conditions.

        if (n > nmax) {
            n <- length(unique(vizData@conds))
        }

        ncp <- min(n, nmax)
    }

    res.pca <- FactoMineR::PCA(vizData@qdata,
                               scale.unit = var.scaling,
                               ncp = ncp,
                               graph = FALSE)
  }

    return(res.pca)
}



#' @param res.pca Result of FactoMineR::PCA
#'
#' @author Samuel Wieczorek, Enora Fremy
#'
#' @export
#' @example examples/ex_mod_ds_pca.R
#'
#' @import highcharter
#'
#' @rdname ds-pca
#'
plotPCA_Eigen <- function(res.pca) {
    stopifnot(!is.null(res.pca))

    hc <- highchart() %>%
        hc_yAxis_multiples(
            list(title = list(text = "% of variances"),
                 lineWidth = 0,
                 labels = list(format = "{value}%"),
                 max = 100),
            list(title = list(text = "Cumulative % of variances"),
                 opposite = FALSE,
                 max = 100
                 ),
            list(title = list(text = "Eigen values"),
                 opposite = TRUE,
                 labels = list(format = "{value}%")
                 )
        ) %>%
        hc_xAxis(title = "Principal Components", 
            categories = rownames(res.pca$eig)) %>%
        hc_add_series(data.frame(y = res.pca$eig[, 2]),
            type = "column",
            name = "% of variances",
            yAxis = 0
        ) %>%
        hc_add_series(data.frame(y = res.pca$eig[, 3]),
            type = "line",
            color = "darkblue",
            name = "Cumulative % of variances",
            marker = "diamond",
            color = "#FF7900",
            yAxis = 0
        ) %>%
        hc_legend(enabled = TRUE)

    hc
}





#' @title Plots variables of PCA
#'
#' @param res.pca xxx
#'
#' @param chosen.axes The dimensions to plot
#'
#' @return A plot
#'
#' @author Samuel Wieczorek
#'
#' @example examples/ex_mod_ds_pca.R
#' 
#'
#' @export
#'
plotPCA_Var <- function(res.pca, chosen.axes = c(1, 2)) {
  pkgs.require('factoextra')
  
  # plot.PCA(res.pca, choix="var", axes = chosen.axes, 
  # title="Sample factor map (PCA)")
  # require(factoextra)
  # Colorer en fonction du cos2: qualit? de repr?sentation
  if (is.null(res.pca)) {
    return(NULL)
  }
  factoextra::fviz_pca_var(
    res.pca,
    axes = chosen.axes, 
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # ?vite le chevauchement de texte
  )
}



#' @title Plots individuals of PCA
#'
#' @param res.pca xxx
#'
#' @param chosen.axes The dimensions to plot
#'
#' @return A plot
#'
#' @author Samuel Wieczorek
#'
#' @example examples/ex_mod_ds_pca.R
#'
#' @export
#'
plotPCA_Ind <- function(res.pca, chosen.axes = c(1, 2)) {
  pkgs.require('factoextra')
  
  if (is.null(res.pca)) {
    return(NULL)
  }
  
  factoextra::fviz_pca_ind(res.pca,
                           axes = chosen.axes, 
                           geom = "point"
                           )
}




#' @title Plots the eigen values of PCA with the highcharts library
#'
#' @param res.pca xxx
#'
#' @return A histogram
#'
#' @author Samuel Wieczorek
#'
#' @example examples/ex_mod_ds_pca.R
#'
#' @import highcharter
#'
#' @export
#'
plotPCA_Eigen_hc <- function(res.pca) {
  if (is.null(res.pca)) {
    return(NULL)
  }
  hc <- highchart() %>%
    hc_yAxis_multiples(
      list(
        title = list(text = "% of variances"), 
        lineWidth = 0, 
        labels = list(format = "{value}%"), max = 100),
      list(
        title = list(text = "Cumulative % of variances"), 
        opposite = FALSE, 
        max = 100),
      list(title = list(text = "Eigen values"), 
           opposite = TRUE, 
           labels = list(format = "{value}%")
      )
    ) %>%
    hc_xAxis(
      title = "Principal Components", 
      categories = rownames(res.pca$eig)) %>%
    hc_add_series(
      data.frame(y = res.pca$eig[, 2]), 
      type = "column", 
      name = "% of variances", 
      yAxis = 0) %>%
    hc_add_series(
      data.frame(y = res.pca$eig[, 3]), 
      type = "line", 
      color = "darkblue", 
      name = "Cumulative % of variances", 
      marker = "diamond", 
      color = "#FF7900", 
      yAxis = 0) %>%
    hc_legend(enabled = TRUE)
}
