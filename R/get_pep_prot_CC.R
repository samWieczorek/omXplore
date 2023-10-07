



#' @title Jitter plot of CC
#'
#' @param list.of.cc List of cc such as returned by the function get.pep.prot.cc
#'
#' @return A plot
#'
#' @author Thomas Burger
#'
#' @examples
#' \donttest{
#' data(vData_ms)
#' plotJitter(vData_ms[1]@cc)
#' }
#'
#' @export
#'
plotJitter <- function(list.of.cc = NULL) {
  if (is.null(list.of.cc)) {
    return()
  }
  
  #x <- length(list.of.cc) # number of CCs
  cc.summary <- sapply(list.of.cc, function(x) {
    c(length(x[[1]]), length(x[[2]]))
  })
  # cc.summary <- vapply(list.of.cc, 
  #     function(x) {c(length(x[[1]]), length(x[[2]]))},
  #     data.frame(2)
  #     )
  
  rownames(cc.summary) <- c("Nb_proteins", "Nb_peptides")
  colSums(cc.summary) # total amount of pep and prot in each CC
  colnames(cc.summary) <- seq_len(length(list.of.cc))
  cc.summary
  rowSums(cc.summary) # c(number of prot, number of pep)
  
  
  cc.summary <- as.data.frame(t(jitter(cc.summary)))
  plot(
    jitter(cc.summary[, 2]), 
    jitter(cc.summary[, 1]), 
    type = "p", 
    xlab = "#peptides in CC", 
    ylab = "#proteins in CC"
  )
}



#' @title Display a CC
#'
#' @param cc A cc (a list)
#' @param peptides_info xxx
#' @param metadata www
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' \donttest{
#' data(vData_ms)
#' obj <- vData_ms[1]
#' g <- buildGraph(obj@cc[[1]], obj@adjMat)
#' }
#'
#' @export
#'
buildGraph <- function(cc,
                       peptides_info = NULL,
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
  
  buildTitle <- function(peptides_info){
    title <- NULL
    if(!is.null(peptides_info)){
      title <- rep('', nb.total)
      for (i in 1:nb.pep){
        ind <- which(rownames(metadata) == rownames(cc)[i])
        title[i] <- paste0("<p>", peptides_info, ':', metadata[ind, peptides_info], "</p>")
      }
  }
    title
  }
   
  nodes <- data.frame(
    id = seq_len(nb.total),
    group = def.grp,
    label = c(rownames(subX), colnames(subX)),
    size = c(rep(10, nb.pep), rep(20, nb.prot)),
    stringsAsFactors = FALSE
  )
  
  title = buildTitle(peptides_info)
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
#' \donttest{
#' data(vData_ms)
#' obj <- vData_ms[1]
#' display.CC.visNet(obj$cc[[1]])
#' }
#'
#' @export
#'
#' @import highcharter
#' @import visNetwork
#'
display.CC.visNet <- function(g,
                              layout = layout_nicely,
                              obj = NULL,
                              prot.tooltip = NULL,
                              pept.tooltip = NULL) {
  
  pkgs.require('visNetwork')
  
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
#' \donttest{
#' data(vData_ms)
#' obj <- vData_ms[1]
#' ll <- obj@cc[1:4]
#' n.prot <- unlist(lapply(ll, function(x) {length(x$proteins)}))
#' n.pept <- unlist(lapply(ll, function(x) {length(x$peptides)}))
#' df <- tibble::tibble(
#' x = jitter(n.pept),
#' y = jitter(n.prot),
#' index = seq_len(length(ll))
#' )
#' plotJitter_rCharts(df)
#' }
#'
plotJitter_rCharts <- function(df, 
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
