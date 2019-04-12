#' Build Network
#'
#' The buildNetwork function runs a Google 'related' search for the input sites
#' or parses the site HTML for hyperlinked sites.
#'
#' @param sites is a vector or json of web sites for the search. 
#' @param searchtype a vector. Type of links search. 'related','links' or 'both' (default is 'related'). 
#' @param snowball a boolean. If TRUE snowball search results (default is FALSE). 
#' @param nodes a data frame. nodes to append to new search to (default is NULL). 
#' @param edges a data frame. edges to append to new search to (default is NULL). 
#' @param excludesites is a vector or json of web sites to be excluded from the search (default is \code{none})
#' @param delay (default is 0)
#' @param maxurls Maximum urls returned in a "related" search (default is 10)
#' @param max_depth Maximum depth a "linked" search will scrape
#' @param time_out time in seconds to set the timeout on how long to wait for a site to respond
#' @param keep_internal a boolean indicating whether to keep all internal pages the site 
#' @param keep_subpages a boolean indicating whether to keep all sub pages pages the site
#' 
#' @return What does this return?
#'
#' @examples \dontrun{
#' buildNetwork(sites='["http://npr.org"]',searchtype = "related")
#' }
#' @export
buildNetwork <- function(sites = sites, searchtype = "related", snowball = FALSE,
                         nodes = NULL, edges = NULL,
                         excludesites = "none", delay = 1, maxurls = 10, max_depth = 5,time_out=100, keep_internal=FALSE, keep_subpages=FALSE) { 
  
  options(stringsAsFactors=F)
  library("dplyr");library("magrittr");
  # requireNamespace("dplyr");requireNamespace("magrittr");
  if( jsonlite::validate(excludesites) ){
    excludesites <- jsonlite::fromJSON(excludesites)
  }

  if( jsonlite::validate(sites) ){
    sites <- jsonlite::fromJSON(sites)
  }
  sites <- unlist(lapply(sites,function(site){if(!grepl("^http",site)) {paste0("http://",site)}else{site}}))

  # loop through sites  
  network <- NULL
  for(site in sites){ # site = sites[1]
    cat(site,"\n")
    if( searchtype == "related" ){
      results <- try(related_urls(x=site,maxurls=maxurls,delay=delay, excludesites = excludesites))
      if(inherits(results,"try-error")) return( paste("Related Error:",results) )
    }else{
      results <- try(linked_urls(x=site,  delay = delay, max_depth = as.integer(max_depth), excludesites = excludesites,time_out=time_out,keep_internal=keep_internal,keep_subpages=keep_subpages))
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
      # nodes$specification <- unlist(lapply(1:nrow(nodes),function(i){paste0('{"',paste(names(nodes[i,"specification"]),nodes[i,"specification"],collapse = '","',sep='":"'),'"}')})) #jsonlite::toJSON(nodes[i,"specification"],auto_unbox=TRUE)}))
      nodes %<>% dplyr::mutate(rooturl=urltools::domain(url))
      edges <- jsonlite::fromJSON(edges)
      # edges$predicate <- unlist(lapply(1:nrow(edges),function(i){jsonlite::toJSON(edges[i,"predicate"],auto_unbox=TRUE)}))
      # edges %<>% 
      #   dplyr::mutate(name_from=urltools::domain(name_from),name_to=urltools::domain(name_to))
 
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
  # if(!keep_internal)
  #   network$nodes  <- network$nodes %>% 
  #     dplyr::filter(!duplicated(rooturl))

  network$nodes <- network$nodes %>% 
    dplyr::mutate(id=seq_along(.data$url)) %>%
    dplyr::mutate(tmp_id=gsub("http://|https://","",url))

  # Update node_from and node_to using new node ids
  if( !is.null( network$edges ) ){

    network$edges <- network$edges %>%
      dplyr::mutate(tmp_to=gsub("http://|https://","",name_to)) %>%
      dplyr::mutate(tmp_from=gsub("http://|https://","",name_from)) %>%
      dplyr::select(dplyr::one_of("tmp_to","tmp_from","name_from","name_to","predicate")) %>%
      dplyr::left_join(network$nodes[,c("id","tmp_id")],by=c("tmp_to" = "tmp_id")) %>% 
      dplyr::rename(node_to=id) %>%
      dplyr::left_join(network$nodes[,c("id","tmp_id")],by=c("tmp_from" = "tmp_id")) %>% 
      dplyr::rename(node_from=id) %>%
      dplyr::mutate(id=seq_along(.data$name_from)) %>%
      dplyr::select(-one_of(c("tmp_to","tmp_from")))

      network$nodes <- network$nodes %>% 
        dplyr::select(-one_of(c("tmp_id")))    
      
      # calculate network graph metrics
      network <- graphMetrics(network)
      
  }else{
    network$edges <- ""
  }

  return( network )

}
