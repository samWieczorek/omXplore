



#' @title Jitter plot of CC
#'
#' @param list.of.cc List of cc such as returned by the function get.pep.prot.cc
#'
#' @return A plot
#'
#' @author Thomas Burger
#'
#' @examples
#' data(Exp1_R25_pept, package="DAPARdata")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' X <- BuildAdjacencyMatrix(obj, "Protein_group_IDs", TRUE)
#' ll <- get.pep.prot.cc(X)
#' plotJitter(ll)
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
#' @param The.CC A cc (a list)
#'
#' @param X xxxxx
#'
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' data(Exp1_R25_pept, package="DAPARdata")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' X <- BuildAdjacencyMatrix(obj, "Protein_group_IDs", FALSE)
#' ll <- get.pep.prot.cc(X)
#' g <- buildGraph(ll[[1]], X)
#'
#' @export
#'
buildGraph <- function(cc) {
  nb.prot <- ncol(The.CC)
  nb.pep <- nrow(The.CC)
  subX <- The.CC
  colnames(subX) <- colnames(The.CC)
  subX <- as.matrix(subX)
  nb.pep.shared <- length(which(rowSums(subX) > 1))
  nb.pep.spec <- length(which(rowSums(subX) == 1))
  nb.total <- nb.prot + nb.pep
  edge.list <- as.data.frame(which(subX == 1, arr.ind = TRUE))
  
  def.grp <- c(rep("shared.peptide", nb.pep), rep("protein", nb.prot))
  def.grp[which(rowSums(subX) == 1)] <- "spec.peptide"
  
  
  nodes <- data.frame(
    id = seq_len(nb.total),
    group = def.grp,
    label = c(rownames(subX), colnames(subX)),
    title = paste0("<p>", seq_len(nb.total), "<br>Tooltip !</p>"),
    size = c(rep(10, nb.pep), rep(20, nb.prot)),
    stringsAsFactors = FALSE
  )
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
#'
#' @param layout xxxxx
#'
#' @param obj xxx
#'
#' @param prot.tooltip xxx
#'
#' @param pept.tooltip xxx
#'
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @examples
#' data(Exp1_R25_pept, package="DAPARdata")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' X <- BuildAdjacencyMatrix(obj, "Protein_group_IDs", FALSE)
#' ll <- get.pep.prot.cc(X)
#' g <- buildGraph(ll[[1]], X)
#' display.CC.visNet(gg)
#'
#' @export
#'
#'
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
  
  
  visNetwork::visNetwork(g$nodes, g$edges, width = "100%", 
                         height = "100%") %>%
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
    visNetwork::visEdges(color = "#A9A9A9", width = 2)
  # %>%
  # visIgraphLayout(layout = "layout_with_fr")
}



#' @title Display a a jitter plot for CC
#'
#' @param df xxxx
#'
#' @param clickFunction xxxx
#'
#' @return A plot
#'
#' @author Thomas Burger, Samuel Wieczorek
#'
#' @export
#' 
#' @examples 
#' data(Exp1_R25_pept, package="DAPARdata")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' X <- BuildAdjacencyMatrix(obj, "Protein_group_IDs", TRUE)
#' ll <- get.pep.prot.cc(X)[1:4]
#' n.prot <- unlist(lapply(ll, function(x) {length(x$proteins)}))
#' n.pept <- unlist(lapply(ll, function(x) {length(x$peptides)}))
#' df <- tibble::tibble(
#' x = jitter(n.pept),
#' y = jitter(n.prot),
#' index = seq_len(length(ll))
#' )
#' plotJitter_rCharts(df)
#'
plotJitter_rCharts <- function(df, clickFunction = NULL) {
  xtitle <- "TO DO"
  
  if (is.null(clickFunction)) {
    clickFunction <-
      JS("function(event) 
                {
                Shiny.onInputChange('eventPointClicked', 
                [this.index]+'_'+ [this.series.name]);
                }")
  }
  
  i_tooltip <- which(startsWith(colnames(df), "tooltip"))
  txt_tooltip <- NULL
  
  if (length(i_tooltip) == 0){
    warning("There is no tooltip in the object.")
  }
  
  for (i in i_tooltip) {
    txt_tooltip <- paste(txt_tooltip, "<b>", gsub("tooltip_", "",
                                                  colnames(df)[i],
                                                  fixed = TRUE
    ),
    " </b>: {point.", colnames(df)[i], "} <br> ",
    sep = ""
    )
  }
  
  h1 <- highchart() %>%
    hc_add_series(data = df, type = "scatter") %>%
    my_hc_chart(zoomType = "xy", chartType = "scatter") %>%
    hc_legend(enabled = FALSE) %>%
    hc_yAxis(title = list(text = "Nb of proteins ic CC")) %>%
    hc_xAxis(title = list(text = "Nb of peptides ic CC")) %>%
    hc_tooltip(headerFormat = "", pointFormat = txt_tooltip) %>%
    hc_plotOptions(series = list(
      animation = list(duration = 100),
      cursor = "pointer",
      point = list(events = list(
        click = clickFunction
      ))
    )) %>%
    my_hc_ExportMenu(filename = "plotCC")
  
  
  return(h1)
}
