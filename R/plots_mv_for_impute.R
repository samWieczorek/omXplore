
#' @param data An instance of a `matrix` containing numeric vales.
#' @param conds A `character()` of the name of conditions (one condition per sample).
#' @param title The title of the plot
#' @param pal.name A `character(1)` which is the name of the palette (from
#' the package [RColorBrewer] to use.
#' 
#' @export
#' 
#' @import highcharter
#' 
#' @rdname plot-mv
#' 
mv.pov.density <- function(data,
                           conds,
                           pal.name = NULL){
  
  if(missing(data))
    stop("'data' is required.")
  
  stopifnot(inherits(data, 'matrix'))
  
  if(missing(conds))
    stop("'conds' is required.")
  
  
  myColors <- NULL
  uconds <- unique(conds)
  palette <- SampleColors(uconds, pal.name)
  mTemp <- nbNA <- nbValues <- matrix(rep(0,nrow(data)*length(uconds)), 
                                      nrow = nrow(data),
                                      dimnames = list(NULL, uconds)
                                      )
  dataCond <- data.frame()
  ymax <- 0
  series <- list()
  myColors <- NULL
  j <- 1
  for (iCond in uconds){
    if (length(which(conds==iCond)) == 1){
      
      mTemp[,iCond] <- data[ ,which(conds==iCond)]
      nbNA[,iCond] <- as.integer(is.OfType(data[,which(conds==iCond)]))
      nbValues[,iCond] <- length(which(conds==iCond)) - nbNA[,iCond]
    } else {
      mTemp[,iCond] <- apply(data[,which(conds==iCond)], 1, mean, na.rm=TRUE)
      nbNA[,iCond] <- apply(data[,which(conds==iCond)],1,function(x) length(which(is.na(x) == TRUE)))
      nbValues[,iCond] <- length(which(conds==iCond)) - nbNA[,iCond]
    }
    
    
    for (i in 1:length(which(conds==iCond))){
      data.tmp <- mTemp[which(nbValues[, iCond] == i), iCond]
      tmp <- NULL    
      if (length(data.tmp) >= 2)
      {
        tmp <- density(mTemp[which(nbValues[,iCond]==i),iCond])
        tmp$y <- tmp$y + i
        if (max(tmp$y) > ymax) { ymax <- max(tmp$y)}
      }
      series[[j]] <- tmp
      myColors <- c(myColors, palette[which(unique(conds)==iCond)])
      j <- j+1
    }
    
  }
  
  axis.title <- list(x = "Mean of intensities",
                     y = "Number of quantity values per condition")
  
  hc <-  highchart() %>%
    hc_title(text = "POV distribution") %>%
    customChart(chartType = "spline", zoomType="xy") %>%
    
    hc_legend(align = "left", 
              verticalAlign = "top",
              layout = "vertical") %>%
    hc_xAxis(title = list(text = axis.title$x)) %>%
    hc_yAxis(title = list(text = axis.title$y),
             tickInterval= 1
             ) %>%
    hc_tooltip(headerFormat= '',
               pointFormat = "<b> {series.name} </b>: {point.y} ",
               valueDecimals = 2) %>%
    customExportMenu(fname = "POV_distribution") %>%
    hc_plotOptions(
      series=list(
        showInLegend = TRUE,
        connectNulls= TRUE,
        marker=list(
          enabled = FALSE)
        
      )
    )
  
  for (i in 1:length(series)){
    hc <- hc_add_series(hc,
                        data = list_parse(data.frame(cbind(x = series[[i]]$x, 
                                                           y = series[[i]]$y))), 
                        showInLegend=FALSE,
                        color = myColors[i],
                        name=conds[i])
  }
  
  # add three empty series for the legend entries. Change color and marker symbol
  for (c in 1:length(uconds)){
    hc <-  hc_add_series(hc,data = data.frame(),
                         name = uconds[c],
                         color = palette[c],
                         marker = list(symbol = "circle"),
                         type = "line")
  }
  
  return(hc)
}



#' @param object An instance of a class `SummarizedExperiment`.
#' @param conds A `character()` of the name of conditions (one condition per sample).
#' 
#' @rdname plot-mv
#' @export
#' @importFrom stats setNames
mv.mec.heatmap <- function(se, conds){
  
  if(missing(se))
    stop("'se' is required.")
  stopifnot(inherits(se, "SummarizedExperiment"))
  
  if(missing(conds))
    stop("'conds' is required.")

  stopifnot('qMetadata' %in% colnames(rowData(se)))
  q.tmp <- rowData(se)[['qMetadata']]
  i_MEC <- which(apply(q.tmp == "missing MEC", 1, sum) >0)
  
  if (length(i_MEC)==0){
    warning("The dataset contains no Missing value on Entire Condition (MEC). 
            So this plot is not available.")
    return(NULL)
  }else if (length(i_MEC)==1){
    warning("The dataset contains only one Missing value on Entire Condition (MEC). 
            Currently, Prostar does not handle such dataset to build the plot. 
            As it has no side-effects on the results, you can continue your imputation.")
    return(NULL)
  }
  
  data <- assay(se)[i_MEC,]
  
  
  ### build indices of conditions
  indCond <- list()
  conds.names <- unique(conds)
  
  indCond <- setNames(lapply(conds.names, 
                             function(x) 
                               grep(x, conds)), 
                      paste0('cond', seq_len(length(conds.names))))
  
  nNA1 = apply(as.matrix(data[,indCond$cond1]), 
               1, 
               function(x) 
                 sum(is.na(x))
               )
  nNA2 = apply(as.matrix(data[,indCond$cond2]), 
               1, 
               function(x) 
                 sum(is.na(x))
               )
  
  o <- order(((nNA1 + 1)^2) / (nNA2 + 1))
  exprso <- data[o,]
  
  for (i in 1:nrow(exprso)){
    k <- order(exprso[i,indCond$cond1])
    exprso[i, rev(indCond$cond1)] <- exprso[i, k]
    .temp <- mean(exprso[i, rev(indCond$cond1)], na.rm = TRUE)
    exprso[i, which(!is.na(exprso[i,indCond$cond1]))] <- .temp
    
    k <- order(exprso[i,indCond$cond2])
    exprso[i, indCond$cond2] <- exprso[i, k+length(indCond$cond1)]
    .temp <- mean(exprso[i, indCond$cond2], na.rm = TRUE)
    exprso[i, length(indCond$cond1) + 
             which(!is.na(exprso[i, indCond$cond2]))] <- .temp
  }
  
  colors <- colorRampPalette(c("yellow", "red"))(100)
  mv.heatmap(x = exprso,
                col = colors,
                key = TRUE,
                srtCol = 0,
                labCol = conds,
                ylab = "Peptides / proteins",
                main = "MEC heatmap"
  )
  
  #heatmap_HC(exprso,col = colfunc(100),labCol=conds)
}

