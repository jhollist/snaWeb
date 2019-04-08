#' Graph Metrics
#'
#' Function for calculating grpah metrics.
#'
#' @param network list with nodes and nodes. 
#'
#' @return What does this return?
#'
#' @examples \dontrun{
#' graphMetrics(network)
#' }
#' @export
graphMetrics <- function(network) { 

#  library(igraph)

  gnodes <- network$nodes
  gedges <- network$edges
  gedges.igraph <- igraph::graph_from_data_frame(d=gedges[, c("node_from", "node_to")], vertices=as.character(gnodes$id)) 

  graphmetrics <- data.frame( id = names(igraph::V(gedges.igraph)),
                              degree = igraph::degree(gedges.igraph), 
                              betweenness = igraph::betweenness(gedges.igraph)
                          )

  gnodes$specification <- unlist(lapply(1:length(gnodes$specification), function(i) {
    spec = jsonlite::fromJSON(gnodes$specification[i])
    spec$degree <- ifelse(is.null(graphmetrics[gnodes$id[i],"degree"]),0,graphmetrics[gnodes$id[i],"degree"])
    spec$betweenness <-  ifelse(is.null(graphmetrics[gnodes$id[i],"betweenness"]),0,graphmetrics[gnodes$id[i],"betweenness"])
    return(jsonlite::toJSON(spec, auto_unbox = TRUE))
  }))
    
  metrics <- list(ap = igraph::average.path.length(gedges.igraph), 
                  diameter = igraph::diameter(gedges.igraph), 
                  reciprocity = igraph::reciprocity(gedges.igraph))

  network$nodes <- gnodes
  network$edges <- gedges
  network$metrics <- metrics
  
  return( network )
}
