
#' @title Display a CC
#'
#' @param cc A cc (a list)
#' @param metadata xxx
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' \donttest{
#' data(vData_ms)
#' obj <- vData_ms[[1]]
#' g <- buildGraph(obj@cc[[1]])
#' }
#'
#' @export
#'
buildGraph <- function(cc,
                       metadata = NULL) {
  nb.prot <- ncol(cc)
  nb.pep <- nrow(cc)
  subX <- cc
  colnames(subX) <- colnames(cc)
  subX <- as.matrix(subX)
  nb.pep.shared <- length(which(rowSums(subX) > 1))
  nb.pep.spec <- length(which(rowSums(subX) == 1))
  nb.total <- nb.prot + nb.pep
  edge.list <- as.data.frame(which(subX == 1, arr.ind = TRUE))
  
  def.grp <- c(rep("shared.peptide", nb.pep), rep("protein", nb.prot))
  def.grp[which(rowSums(subX) == 1)] <- "spec.peptide"
  
  buildNodesInfos <- function(metadata){
    nodes_infos <- NULL
    if(!is.null(metadata)){
      nodes_infos <- rep('', nb.total)
      
      # We add infos only on peptides nodes
      for (i in 1:nb.pep){
        ind <- which(rownames(metadata) == rownames(cc)[i])
        nodes_infos[i] <- paste0("<p>", colnames(metadata)[i], ':', metadata[ind, ], "</p>")
      }
  }
    nodes_infos
  }
   
  nodes <- data.frame(
    id = seq_len(nb.total),
    group = def.grp,
    label = c(rownames(subX), colnames(subX)),
    size = c(rep(10, nb.pep), rep(20, nb.prot)),
    stringsAsFactors = FALSE
  )
  
  title = buildNodesInfos(metadata)
  if (!is.null(title))
    nodes <- cbind(nodes, title)
  
  edges <- data.frame(
    from = c(edge.list$row),
    to = c(edge.list$col + nb.pep),
    stringsAsFactors = FALSE
  )
  
  return(
    list(
      nodes = nodes, 
      edges = edges
    )
  )
}


#' @title Display a CC
#'
#' @param g A cc (a list)
#' @param layout xxxxx
#' @param obj xxx
#' @param prot.tooltip xxx
#' @param pept.tooltip xxx
#'
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' data(vData_ft)
#' cc <- vData_ft[[1]]@cc[[1]]
#' g <- buildGraph(cc) 
#' display.CC.visNet(g)
#' 
#'
#' @export
#'
#' @import highcharter
#' @import visNetwork
#'
display.CC.visNet <- function(g,
                              layout = NULL,
                              obj = NULL,
                              prot.tooltip = NULL,
                              pept.tooltip = NULL) {
  
  pkgs.require(c('visNetwork', 'igraph'))
  
  if(is.null(layout))
    layout <- igraph::layout_nicely
  
  col.prot <- "#ECB57C"
  col.spec <- "#5CA3F7"
  col.shared <- "#0EA513"

  visNetwork::visNetwork(g$nodes, g$edges, width = "100%", height = "100%") %>%
    visNetwork::visNodes(shape = "dot") %>% # square for all nodes
    visNetwork::visGroups(groupname = "spec.peptide", 
                          color = col.spec) %>% # darkblue for group "A"
    visNetwork::visGroups(groupname = "shared.peptide", 
                          color = col.shared) %>% # darkblue for group "A"
    visNetwork::visGroups(groupname = "protein", 
                          color = col.prot, shape = "dot") %>%
    visNetwork::visOptions(highlightNearest = FALSE) %>%
    # visLegend()
    # visPhysics(stabilization = FALSE)%>%
    visNetwork::visEdges(color = "#A9A9A9", width = 2) %>%
    visNetwork::visIgraphLayout(layout = "layout_with_fr")
}



#' @title Display a jitter plot for connected components
#'
#' @param df xxxx
#' @param clickFunction xxxx
#'
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @export
#' @import highcharter
#' 
#' @examples 
#' data(vData_ft)
#' obj <- vData_ft[[1]]
#' 
#' n.prot <- unlist(lapply(obj@cc, function(x) {ncol(x)}))
#' n.pept <- unlist(lapply(obj@cc, function(x) {nrow(x)}))
#' df <- tibble::tibble(x = jitter(n.pept),
#'                     y = jitter(n.prot),
#'                     index = seq_len(length(obj@cc)))
#' plotCCJitter(df)
#'
plotCCJitter <- function(df,
                         clickFunction = NULL) {
  
  if (is.null(clickFunction)) {
    clickFunction <-
      JS("function(event){Shiny.onInputChange('eventPointClicked', 
          [this.index]+'_'+ [this.series.name]);}")
  }
  
 # i_tooltip <- which(startsWith(colnames(df), "tooltip"))
  txt_tooltip <- NULL
  
  #if (length(i_tooltip) == 0){
  #  warning("There is no tooltip in the object.")
 # } else {
    # for (i in i_tooltip) {
    #   txt_tooltip <- paste(txt_tooltip, "<b>", 
    #                        gsub("tooltip_", "", colnames(df)[i], fixed = TRUE),
    #                        " </b>: {point.", colnames(df)[i], "} <br> ",
    #                        sep = "")
    # }
  #}
  
  highcharter::highchart() %>%
    highcharter::hc_add_series(data = df, type = "scatter") %>%
    customChart(zoomType = "xy", chartType = "scatter") %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_yAxis(title = list(text = "Nb of proteins")) %>%
    highcharter::hc_xAxis(title = list(text = "Nb of peptides")) %>%
    highcharter::hc_tooltip(enabled = FALSE,
                            headerFormat = "", 
                            pointFormat = txt_tooltip) %>%
    highcharter::hc_plotOptions(series = list(
      animation = list(duration = 100),
      cursor = "pointer",
      point = list(events = list(click = clickFunction))
    )) %>%
    customExportMenu(fname = "plotCC")
  
}
