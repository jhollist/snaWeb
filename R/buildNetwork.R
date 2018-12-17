#' Build Network
#'
#' The buildNetwork function runs a Google 'related' search for the input sites
#' or parses the site HTML for hyperlinked sites.
#'
#' @param sites a vector. Web sites for the Goolge related search. 
#' @param searchtype a vector. Type of links search. 'related','links' or 'both' (default is 'related'). 
#' @param snowball a boolean. If TRUE snowball search results (default is FALSE). 
#' @param nodes a data frame. nodes to append to new search to (default is NULL). 
#' @param edges a data frame. edges to append to new search to (default is NULL). 
#' @param excludesites (default is \code{NULL})
#' @param delay (default is 0)
#' @param maxurls Maximum urls returned in a "related" search (default is 10)
#' @param max_depth Maximum depth a "linked" search will scrape
#'
#' @return What does this return?
#'
#' @examples \dontrun{
#' buildNetwork(sites='["http://npr.org"]',searchtype = "related")
#' }
#' @export
buildNetwork <- function(sites = sites, searchtype = "related", snowball = FALSE,
                         nodes = NULL, edges = NULL,
                         excludesites = "none", delay = 1, maxurls = 10, max_depth = 5) { 
  
  options(stringsAsFactors=F)
  #requireNamespace("dplyr");requireNamespace(magrittr);
  if( excludesites != "none" )
    excludesites <- jsonlite::fromJSON(excludesites)
  sites <- jsonlite::fromJSON(sites)
  sites <- unlist(lapply(sites,function(site){if(!grepl("^http",site)) {paste0("http://",site)}else{site}}))

  # loop through sites  
  network <- NULL
  for(site in sites){ # site = sites[1]
    if( searchtype == "related" ){
      results <- try(related_urls(x=site,maxurls=maxurls,delay=delay, excludesites = excludesites))
      if(inherits(results,"try-error")) return( paste("Related Error:",results) )
    }else{
      results <- try(linked_urls(x=site,  delay = delay, max_depth = as.integer(max_depth), excludesites = excludesites))
      if(inherits(results,"try-error")) return( paste("Linked Error: site",site,"excludesites",jsonlite::toJSON(excludesites),results) )
    }
    # update the network with the latest site's results
    if( !is.null(network) ){
      network$nodes %<>% 
        dplyr::bind_rows(results$nodes)
      network$edges %<>%
        dplyr::bind_rows(results$edges)
      
    }else{
      network <- results
    }
  }

  # Add the nodes and edges passsed in to the network
  if( !is.null(nodes) ){
    if( nchar(nodes) > 3 ){ # nchar > '[ ]'
      # Assume the root node comes from the nodes passed in.
      network$nodes %<>% dplyr::mutate(is_root=replace(is_root,is_root==TRUE,FALSE))
    
      nodes <- jsonlite::fromJSON(nodes,simplifyVector = TRUE)
      nodes$specification <- unlist(lapply(1:nrow(nodes),function(i){paste0('{"',paste(names(nodes[i,"specification"]),nodes[i,"specification"],collapse = '","',sep='":"'),'"}')})) #jsonlite::toJSON(nodes[i,"specification"],auto_unbox=TRUE)}))
      nodes %<>% dplyr::mutate(rooturl=urltools::domain(url))
      edges <- jsonlite::fromJSON(edges)
      edges$predicate <- unlist(lapply(1:nrow(edges),function(i){jsonlite::toJSON(edges[i,"predicate"],auto_unbox=TRUE)}))
      edges %<>% 
        dplyr::mutate(name_from=urltools::domain(name_from),name_to=urltools::domain(name_to))
  
      network$nodes <-
        dplyr::bind_rows(nodes,network$nodes)
    
      if( !is.null( network$edges ) ){
        network$edges <- 
          dplyr::bind_rows(edges,network$edges)
      }else{
        network$edges <- edges
      }
    }
  }

  # Redo ids
  network$nodes %<>% 
    dplyr::filter(!duplicated(rooturl)) %>%
    dplyr::mutate(id=seq_along(.data$url))

  # Update node_from and node_to using new node ids
  if( !is.null( network$edges ) ){
    
    network$edges %<>%
      dplyr::select(dplyr::one_of("name_to","name_from","predicate")) %>%
      dplyr::left_join(network$nodes[,c("id","rooturl")],by=c("name_to" = "rooturl")) %>% 
      dplyr::rename(node_to=id) %>%
      dplyr::left_join(network$nodes[,c("id","rooturl")],by=c("name_from" = "rooturl")) %>% 
      dplyr::rename(node_from=id) %>%
      dplyr::mutate(id=seq_along(.data$name_from))
    
      # calculate network graph metrics
      network <- graphMetrics(network)
      
  }else{
    network$edges <- ""
  }

  return( network )

}
