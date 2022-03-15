#' @title Connected components of a graph.
#'
#' @description
#' 
#' This function computes the connex components of a graph, 
#' represented by its adjacency matrix.
#'
#' @param obj.se An instance of the class `SummarizedExperiment`. This object
#' must have a adjacencyMatrix.
#' 
#' @param X A `Matrix` which is the adjacency matrix.
#' 
#' @param cc A `list()` which contains the connected components
#' 
#' @param g A `list()` formed to store a graph with its nodes and edges.
#' 
#' @seealso The `adjacencyMatrix()` function of the package `QFeatures`.
#'
#' @author Thomas Burger, Samuel Wieczorek
#' 
#' @name connected-components
#'
#' @examples
#' data(ft)
#' X <- adjacencyMatrix(ft[[1]])
#' 
#' #------------------------------------------
#' # Build the cc list for an adjaceny matrix
#' #------------------------------------------
#' 
#' ll.cc <- compute.cc(X)
#' 
#' #-------------------------------------------------
#' # Convert one cc of this list to a graph structure
#' #-------------------------------------------------
#' 
#' g <- convertCC2graph(ll.cc[[1]], X)
#' 
#' #-------------------------------------------
#' # Build the cc list for all type of peptides 
#' # 'all', 'shared', 'specific'
#' #-------------------------------------------
#' 
#' ll.cc <- wrapper.compute.cc(ft[[1]])
#'
#' #-------------------------------------------------
#' # Show the graph
#' #-------------------------------------------------
#'  
#' show.graph(g)
#' 
#' 
#' #-----------------------------------------------------
#' # Plots a graph which x-axis is the number of peptides
#' # in the cc and the y-axis is the number of proteins
#' # in the cc.
#' #'-----------------------------------------------------
#'
#' tt <- as.data.frame(rowData(ft[[1]])[,1])
#' colnames(tt) <- colnames(rowData(ft[[1]]))[1]
#' plotJitter_hc(ll.cc, tt)
#' plotJitter_hc(ll.cc)
#' 
NULL



#' @export
#'
#' @rdname connected-components
#'
wrapper.compute.cc <- function(obj.se){
  
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop(
      "Package \"SummarizedExperiment\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("QFeatures", quietly = TRUE)) {
    stop(
      "Package \"QFeatures\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  stopifnot('adjacencyMatrix' %in% colnames(SummarizedExperiment::rowData(obj.se)))

  res <- list(all = NULL,
              onlyShared = NULL,
              onlySpec = NULL)

  ll.X <- QFeatures::adjacencyMatrix(obj.se)

  if (!is.null(ll.X$all))
    res$all <- compute.cc(ll.X$all)


  if (!is.null(ll.X$onlyShared))
    res$onlyShared <- compute.cc(ll.X$onlyShared)

  if (!is.null(ll.X$onlySpec))
    res$onlySpec <- compute.cc(ll.X$onlySpec)

  return (res)
}



#' @export
#'
#' @importFrom methods as
#'
#' @rdname connected-components
#'
compute.cc <- function(X){
  
  if (! requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install Matrix: BiocManager::install('Matrix')")
  }
  
  if (! requireNamespace("graph", quietly = TRUE)) {
    stop("Please install graph: BiocManager::install('graph')")
  }
  
  stopifnot(!is.null(X))

  p <- dim(X)[2] # Nb proteins
  q <- dim(X)[1] # Nb peptides

  multprot.cc <- singprot.cc <- multprot.cc.pep <- singprot.cc.pep <- NULL
  A <- B <- g <- NULL
  ### Adjacency matrix construction
  # boolean matrix product
  print("Start computing boolean matrix product")
  A <- Matrix::crossprod(X, boolArith = TRUE)
  # A <- as.matrix(t(X) %*% X)
  # A <- as.matrix(t(X) %&% X)
  print("End of computing boolean matrix product")

  # remove self-connecting edges
  diag(A) <- rep(0,p)
  A <- matrix(as.numeric(A[,]), ncol=p) # goes back to classical matrix format
  colnames(A) <- rownames(A) <- colnames(X) # reset pep and prot names

  # proteins with no shared peptides
  SingleProt.CC.id <- which(rowSums(A)==0)
  if (length(SingleProt.CC.id) > 0){
    ### Peptides from single prot CCs
    singprot.cc <- as.list(names(SingleProt.CC.id))
    singprot.cc.pep <- list()
    for(i in seq_len(length(singprot.cc))){
      peplist <- which(X[, singprot.cc[[i]]]!=0)
      singprot.cc.pep[[i]] <- names(peplist)
    }
  }


  if (length(SingleProt.CC.id) < nrow(A)){
    # matrix with no 1-prot CC
    B <- A[-SingleProt.CC.id, -SingleProt.CC.id] 

    ### Protein CCs
    multprot.cc <- NULL
    g <- graph::graphAM(B, edgemode = 'undirected', values=NA)
    multprot.cc <- graph::connComp(as(g, 'graphNEL'))

    ### Peptides from multiple prot CCs
    multprot.cc.pep <- list()
    for(i in seq_len(length(multprot.cc))){
      protlist <- multprot.cc[[i]]
      subX <- as.matrix(X[, protlist])
      peplist <- which(rowSums(subX)!=0)
      multprot.cc.pep[[i]] <- names(peplist)
    }
  }


  ### Merge results into a single list
  prot.cc <- c(multprot.cc, singprot.cc)
  pep.cc <- c(multprot.cc.pep, singprot.cc.pep)
  global.cc <- list()
  for(i in seq_len(length(prot.cc))){
    prot <- prot.cc[[i]]
    pep <- pep.cc[[i]]
    tmp <- list(prot,pep)
    names(tmp) <- c("proteins", "peptides")
    global.cc[[i]] <- tmp
  }

  ### Clean memory and return result
  rm(A,
     B,
     g,
     multprot.cc,
     singprot.cc,
     multprot.cc.pep, 
     singprot.cc.pep,
     pep,prot.cc,
     pep.cc)
  gc()
  return(global.cc)
}



#'
#' @export
#'
#' @rdname connected-components
#'
convertCC2graph <- function(cc, X){

  subX <- as.matrix(X[cc$peptides, cc$proteins])
  nb.prot <- length(cc$proteins)
  nb.pep <- length(cc$peptides)
  nb.pep.shared <- length(which(rowSums(subX)>1))
  nb.pep.spec <- length(which(rowSums(subX)==1))
  nb.total = nb.prot + nb.pep
  edge.list <- as.data.frame(which(subX == 1, arr.ind = TRUE))

  def.grp <- c(rep("shared.peptide", nb.pep), 
               rep("protein", nb.prot)
               )
  def.grp[which(rowSums(subX) == 1)] <- 'spec.peptide'


  nodes <- data.frame(id = seq_len(nb.total),
                      group = def.grp,
                      label = c(rownames(subX), colnames(subX)),
                      title =  paste0("<p>", seq_len(nb.total),"<br>Tooltip !</p>"),
                      size = c(rep(10, nb.pep),rep(20, nb.prot)),
                      stringsAsFactors = FALSE
  )
  edges <- data.frame(from = c(edge.list$row),
                      to = c(edge.list$col+ nb.pep),
                      stringsAsFactors = FALSE)

  return(list(nodes = nodes, 
              edges = edges)
         )
}


#' @param obj xxx
#' @param g xxx
#' @param prot.tooltip A `character()` which are the tooltips of the proteins.
#' @param pept.tooltip A `character()` which are the tooltips of the peptides.
#' @export
#'
#' @rdname connected-components
#'
show.graph <- function(g,
                       obj = NULL,
                       prot.tooltip = NULL,
                       pept.tooltip = NULL){

  if (! requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Please install visNetwork: BiocManager::install('visNetwork')")
  }
  
  
  col <- list(prot = "#ECB57C",
              spec = "#5CA3F7",
              shared = "#0EA513",
              edges = "#A9A9A9")


  visNetwork::visNetwork(g$nodes, 
                         g$edges, 
                         width = "100%", 
                         height = "100%") %>%
    # square for all nodes
    visNetwork::visNodes(shape = "dot") %>%
    # darkblue for group "A"
    visNetwork::visGroups(groupname = "spec.peptide", 
                          color = col$spec) %>%
    # darkblue for group "A"
    visNetwork::visGroups(groupname = "shared.peptide", 
                          color = col$shared) %>%    
    visNetwork::visGroups(groupname = "protein", 
                          color = col$prot, 
                          shape = "dot") %>%
    visNetwork::visOptions(highlightNearest = FALSE) %>%
    #visLegend()
    #visPhysics(stabilization = FALSE)%>%
    visNetwork::visEdges(color = col$edges,
                         width = 2)


}



#' @param tooltips A `data.frame` which contains a subset of the rowData
#' of the object and will be use as tooltips. Each column of the data.frame
#' will be showed in the global tooltip of a point in the plot.
#' 
#' @param ll.cc A dataframe containing xxx
#'
#' @param clickFunction A JavaScript code to show an information when a point is clicked
#' @export
#'
#' @import highcharter
#' @importFrom tibble tibble
#'
#' @rdname connected-components
#'
plotJitter_hc <- function(ll.cc,
                          df.tooltips = NULL,
                          clickFunction = NULL){

  
  n.prot <- unlist(lapply(ll.cc, function(x){length(x$proteins)}))
  n.pept <- unlist(lapply(ll.cc, function(x){length(x$peptides)}))
  
  df <- tibble::tibble(x = jitter(n.pept),
                       y = jitter(n.prot),
                       index = seq_len(length(ll.cc))
                       )
  # Add tooltips
  txt_tooltips <- NULL
  if (!is.null(df.tooltips)){
    stopifnot(inherits(df.tooltips, 'data.frame'))
    stopifnot(length(dim(df.tooltips)) == 2 && length(colnames(df.tooltips)) > 0)
    df <- cbind(df, df.tooltips)
    
    # Affect only tooltips columns
    ind <- seq(4, ncol(df))
    colnames(df)[ind] <- paste("tooltip_", colnames(df)[ind], sep="")
    
    for (i in 4:ncol(df))
      txt_tooltips <- paste(txt_tooltips,
                            "<b>",
                            gsub("tooltip_", "", colnames(df)[i], fixed=TRUE),
                            " </b>: {point.", colnames(df)[i],"} <br> ",
                            sep="")
  }

  if (is.null(clickFunction)){
    clickFunction <-
      JS(paste0("function(event) {",
      "Shiny.onInputChange('eventPointClicked',",
      "[this.index]+'_'+ [this.series.name]);}"))
  }

  
  
  h1 <-  highchart() %>%
    hc_add_series(data = df, type = "scatter") %>%
    customChart(zoomType = "xy", chartType="scatter") %>%
    hc_legend(enabled = FALSE) %>%
    hc_yAxis(title = list(text="Nb of proteins in CC")) %>%
    hc_xAxis(title = list(text = "Nb of peptides in CC")) %>%
    hc_plotOptions(series = list( animation = list(duration = 100),
                                  cursor = "pointer",
                                  point = list( events = list(
                                    click = clickFunction ) ) ) ) %>%
    customExportMenu(fname = "plotCC")

if (!is.null(txt_tooltips))
  h1 <- h1 %>% hc_tooltip(headerFormat = '', pointFormat = txt_tooltips)
  
  return(h1)
}


#' @param ll.cc A `list()` of xxx
#' @param type A `character(1)` which denotes the type ofconnected components
#' to extract. Available values are: 'OneOne', 'OneMulti' and 'MultiAny'.
#' @rdname connected-components
#' @export
Extract_CC <- function(ll.cc, type){
  
  if(missing(type)){
    warning('type is missing. Returns the cc list.')
    return(ll.cc)
  }
  
  stopifnot(type %in% c('OneOne', 'OneMulti', 'MultiAny'))
  
  ll.prot <- lapply(ll.cc, function(x){length(x$proteins)})
  ll.pept <- lapply(ll.cc, function(x){length(x$peptides)})
  
  ind.res <- switch(type,
                OneOne = intersect(which(ll.prot == 1), which(ll.pept == 1)),
                OneMulti = intersect(which(ll.prot == 1), which(ll.pept > 1)),
                MultiAny = which(ll.prot > 1))
  
  ll.cc[ind.res]
}


