
#' @param data An instance of the class `SummarizedExperiment`
#' @param conds xxx
#' @param var.scaling The dimensions to plot
#' @param ncp A `integer(1)` which represents the umber of dimensions kept in the results.
#' 
#' @importFrom stats na.omit
#' 
#' @export
#' 
#' @rdname ds-pca
#' 
wrapper_pca <- function(data, 
                        conds, 
                        var.scaling = TRUE, 
                        ncp = NULL){
  
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop(
      "Package \"FactoMineR\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  if(missing(data))
    stop("'data' is missing.")
  if(missing(conds))
    stop("'conds' is missing.")
  
  stopifnot(inherits(data, 'matrix'))
  
  if (is.null(var.scaling))
    var.scaling <- TRUE
  
  if (length(which(is.na(data))) > 0)
    data <- stats::na.omit(data)
  
  
  if (is.null(ncp)){
    nmax <- 12
    y <- data
    nprot <- dim(y)[1]
    n <- dim(y)[2] # If too big, take the number of conditions.
    
    if (n > nmax)
      n <- length(unique(conds))
    
    ncp <- min(n, nmax)
  }
  
  res.pca <- FactoMineR::PCA(data,
                             scale.unit = var.scaling, 
                             ncp = ncp, 
                             graph = FALSE
                             )
  
  return(res.pca)
}



#' @param res.pca Result of FactoMineR::PCA
#' 
#' @author Samuel Wieczorek, Enora Fremy
#' 
#' @export
#' 
#' @import highcharter
#' 
#' @rdname ds-pca
#' 
plotPCA_Eigen <- function(res.pca){
  stopifnot(!is.null(res.pca))
  
  hc <- highchart() %>%
    hc_yAxis_multiples(list(title = list(text = "% of variances"),
                            lineWidth = 0, 
                            labels = list(format = "{value}%"), 
                            max = 100), 
                       list(title = list(text = "Cumulative % of variances"), 
                            opposite = FALSE, 
                            max = 100),
                       list(title = list(text = "Eigen values"), 
                            opposite = TRUE, 
                            labels = list(format = "{value}%") )
                       ) %>%
    hc_xAxis(title = "Principal Components", categories = rownames(res.pca$eig)) %>%
    hc_add_series(data.frame(y = res.pca$eig[,2]), 
                  type = "column",  
                  name = "% of variances", 
                  yAxis = 0) %>%
    hc_add_series(data.frame(y = res.pca$eig[,3]), 
                  type = "line",  
                  color="darkblue",
                  name = "Cumulative % of variances", 
                  marker = "diamond", 
                  color = "#FF7900", 
                  yAxis = 0) %>%
    hc_legend(enabled = TRUE)
  
  hc
}
