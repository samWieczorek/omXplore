#' @param data xxx
#' @export 
#' @import highcharter
#' @rdname plot-mv
mvPerLinesHisto <- function(data){
  stopifnot(inherits(data, "matrix"))
  coeffMax <- .1
  
  NbNAPerCol <- colSums(is.na(data))
  NbNAPerRow <- rowSums(is.na(data))
  
  nb.col <- dim(data)[2] 
  nb.na <- NbNAPerRow
  temp <- table(NbNAPerRow)
  nb.na2barplot <- c(temp, rep(0, 1 + ncol(data) - length(temp)))
  
  if (sum(NbNAPerRow) == 0)
    nb.na2barplot <- rep(0, 1 + ncol(data))
  
  df <- data.frame(y = nb.na2barplot[-1])
  myColors = rep("lightgrey", nrow(df))
  myColors[nrow(df)] <- "red"

  h1 <-  highchart() %>% 
    hc_title(text = "#[lines] with X NA values") %>% 
    hc_add_series(data = df, type = "column", colorByPoint = TRUE) %>%
    hc_colors(myColors) %>%
    hc_plotOptions(column = list(stacking = "normal"),
                    animation = list(duration = 100)) %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = row.names(df), title = list(text = "#[NA values] per line")) %>%
    customExportMenu(fname = "mvPlot1") %>%
    hc_tooltip(enabled = TRUE,
               headerFormat = '',
               pointFormat = "{point.y} ")
  
  return(h1)
  
}



#' @param data xxxx
#' @param conds A `character()` of the name of conditions 
#' (one condition per sample). The number of conditions must be equal to
#' the number of samples (number of columns) of the parameter 'data'.
#' @param pal.name A `character(1)` which is the name of the palette from the package
#' [RColorBrewer] from which the colors are taken. Default value is 'Set1'.
#' @export
#' 
#' @import highcharter
#' @rdname plot-mv
mvPerLinesHistoPerCondition <- function(data, 
                                        conds, 
                                        pal.name){
  
  stopifnot(inherits(data, "matrix"))
  myColors <-  SampleColors(conds, pal.name)
  nbConditions <- length(unique(conds))
  
  ncolMatrix <- max(unlist(lapply(unique(conds), 
                                  function(x){length(which(conds==x))})))
  m <- matrix(rep(0, nbConditions*(1+ncolMatrix)), 
              ncol = nbConditions, 
              dimnames = list(seq(0:(ncolMatrix)), unique(conds)))
  
  for (i in unique(conds)) {
    nSample <- length(which(conds == i))
    t <- NULL
    if (nSample == 1) {
      t <- table(as.integer(is.na(data[,which(conds == i)])))
    } else {
      t <- table(rowSums(is.na(data[ ,which(conds == i)])))
      }
    
    m[as.integer(names(t))+1, i] <- t
  }
  
  m <- as.data.frame(m)
  
  rownames(m) <- 0:(nrow(m)-1)
  
  h1 <-  highchart() %>% 
    hc_title(text = "#[lines] with X NA values (condition-wise)") %>% 
    customChart(chartType = "column") %>%
    hc_plotOptions( column = list(stacking = ""),
                    dataLabels = list(enabled = FALSE),
                    animation = list(duration = 100)) %>%
    hc_colors(unique(myColors)) %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = row.names(m), title = list(text = "#[NA values] per line (condition-wise)")) %>%
    customExportMenu(fname = "mvPlot_2") %>%
    hc_tooltip(headerFormat= '',
               pointFormat = "{point.y} ")
  
  for (i in seq_len(nbConditions)){
    h1 <- h1 %>% hc_add_series(data=m[ ,unique(conds)[i]]) }
  
  
  return(h1)
  
}




#' @param data xxxx
#' @param conds A `character()` of the name of conditions 
#' (one condition per sample). The number of conditions must be equal to
#' the number of samples (number of columns) of the parameter 'data'.
#' @param showValues A `logical()` that indicates whether to show 
#' values in the plot.
#' @param pal.name A `character(1)` which is the name of the palette from the package
#' [RColorBrewer] from which the colors are taken. Default value is 'Set1'.
#' @export
#' @import highcharter
#' @rdname plot-mv
mvHisto <- function(data,
                    conds,
                    showValues = FALSE, 
                    pal.name = NULL){
  
  stopifnot(inherits(data, "matrix"))
  
  myColors <-  SampleColors(conds, pal.name)
  NbNAPerCol <- colSums(is.na(data))
  NbNAPerRow <- rowSums(is.na(data))
  
  df <- data.frame(NbNAPerCol)
  names(df) <- 'y'
  
  
  h1 <-  highchart() %>%
    customChart(chartType = "column") %>%
    hc_title(text = "#NA by replicate") %>%
    hc_add_series(df, type="column", colorByPoint = TRUE) %>%
    hc_colors(myColors) %>%
    hc_plotOptions( column = list(stacking = "normal"),
                    animation=list(duration = 100)) %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = conds, title = list(text = "Replicates")) %>%
    customExportMenu(fname = "mvPlot_3") %>%
    hc_tooltip(headerFormat= '',
               pointFormat = "{point.y}")
  
  
  return(h1)
  
}


