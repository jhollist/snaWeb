#' Social Network Related Sites
#'
#' Use Google's related:site advance search feature to fine websites related to
#' one or more sites of interest.
#'
#' Performing searches on Google via a program can be difficult.  Google, and
#' many other search engines, monitor network traffic and will flag traffic that
#' might be coming from a bot.  Making several queries very quickly can result
#' in the queries being flag.  To address this issue we have a \code{delay}
#' argument to pause queries by a given number of seconds.
#'
#' A preferable method to using the delay, and a method that *might* be
#' implemented at a later date, is to use the Google Custom Search JSON/Atom
#' API, \url{https://developers.google.com/custom-search/json-api/v1/overview}.
#' As of August 30, 2017, the "API provides 100 searches per day for free...
#' additional requests cost $5 per 1,000 queries up to 10k queries per day."
#'
#' TODO: describe passing a timeout option via html_session, httr::config, ...
#'
#' @param x The url, as a character string, to base the google related site
#' serarch.
#' @param maxurls an integer value specifying the maximum number of related
#' sites to return.
#' @param delay minimum number of seconds to delay between sending queries to
#' Google.  The delay is needed to avoid having Google detect the queries as
#' coming from a bot.
#' @param excludesites (default is \code{NULL})
#' @param ... passed to \code{\link[rvest]{html_session}}
#'
#' @return A \code{sna_sites} object.  This is a list with four elements:
#' \describe{
#' \item{nodes}{A \code{data.frame} describing the nodes of a graph
#'   \describe{
#'   \item{url}{url of the node}
#'   \item{is_root}{A character (looks like a logical, but is a character)
#'   with values "TRUE" and "FALSE" denoting if the node is a root (parent) node
#'   in the graph}
#'   \item{id}{node id}
#'   \item{name}{title of the website}
#'   \item{specification}{}
#'   }
#' }
#' \item{edges}{A \code{data.frame} describing the edges of a graph
#'   \describe{
#'   \item{name_from}{url of the parent (site) node for the edge}
#'   \item{name_to}{url of the child (site) note for the edge}
#'   \item{id}{edge id}
#'   \item{node_from}{node id of the parent node}
#'   \item{node_to}{node id of the child node}
#'   \item{rank}{The order the site was listed in the google search results.}
#'   \item{predicate}{}
#'   }
#'   }
#' \item{message}{a character string with the value "Blocked" or "Success."}
#' \item{is_blocked}{a logical of legnth 1}
#' }
#'
#' @examples
#' \dontrun{
#' related_urls("neptuneinc.org")
#' }
#'
#' @export
related_urls <- function(x, maxurls = 10L, delay = 2, excludesites=NULL, ...) {
  UseMethod("related_urls")
}

#' @export
related_urls.character <- function(x, maxurls = 10L, delay = 2, excludesites=NULL, ...) {
  requireNamespace("magrittr")
  strt <- 0
  related_sites <- vector('character', length = 0)
  
  while(length(related_sites) < maxurls) {
    # construct the google_query for related sites
    google_query <-
      sprintf("http://www.google.com/search?hl=en&lr=&ie=ISO-8859-1&q=related:%s&btnG=Search&ndplr=1&start=%d",
              x, strt)
    
    session <- rvest::html_session(google_query, ...)
    
    this_url_set <-
      get_hrefs(session,
                keep_regex = "\\/url\\?q\\=",
                omit_regex = "webcache.googleusercontent.co")
    
    this_url_set$url %<>% gsub("^.*\\/url\\?q=(.*)&sa.*$", "\\1", .)
    if( !is.null(excludesites) )
      this_url_set <- this_url_set[!grepl(paste0(excludesites,collapse="|"),this_url_set$url),]
    
    related_sites <- c(related_sites, this_url_set$url)
    
    if (nrow(this_url_set) < 10L) {
      break
    } else {
      strt <- strt + 10
      Sys.sleep(delay)
    } 
  } # end while loop
  
  if( length(related_sites)!=0 ){
    related_sites <- related_sites[seq(1, min(length(related_sites), maxurls), by = 1)]
  }
  
  # generate the nodes data.frame
  nodes <-
    dplyr::data_frame(url = c(x, related_sites),
                      is_root = c("TRUE", rep("FALSE", length(related_sites)))
    ) %>%
    dplyr::mutate(id = seq_along(.data$url)) %>%
    dplyr::mutate(rooturl = urltools::domain(.data$url))
  
  nodes$name <- lapply(nodes$url, get_page_title) %>% do.call(c, .)
  nodes$specification <-
    with(nodes, sprintf('{"is_root":"%s","url":"%s","name":"%s"}', is_root, url, name))
  
  # build the data frame to define the edges of the social network graph.
  # edges <-
  #   dplyr::data_frame(name_from = x,
  #                     name_to   = related_sites,
  #                     id = seq_along(.data$name_to)
  #                     ) %>% 
  #   dplyr::left_join(.,
  #                    dplyr::select(nodes, .data$url, node_to = .data$id),
  #                    by = c("name_to" = "url")) %>%
  #   dplyr::left_join(.,
  #                    dplyr::select(nodes, .data$url, node_from = .data$id),
  #                    by = c("name_from" = "url")) %>%
  #   dplyr::mutate(rank = .data$node_to - .data$node_from)
  
  if( length(related_sites) != 0 ){
    edges <-
      dplyr::data_frame(name_from = urltools::domain(x),
                        name_to   = urltools::domain(related_sites))
    # dplyr::mutate(predicate=sprintf('{"predicate":"is related","rank":"%d"}', row.names(.data)))
    
    edges$predicate <- sprintf('{"predicate":"is related","rank":"%d"}', as.integer(row.names(edges)))
  } else{
    edges <- NULL
  }
  # with(edges, sprintf('{"predicate":"is related","rank":"%d"}', as.integer(row.names(tmp))))
  
  # return object
  out <-
    list(nodes      = nodes,
         edges      = edges,
         message    = "Success",  # Hold over form older version... 
         is_blocked = FALSE)      # Hold over form older version... 
  # class(out) <- c("sna_related_urls", "sna_urls")
  out
}

google <- function(site, maxurls, delay, excludesites) {
  # The Google search string below will return ten at a time and indexed
  # starting with 0.  strt will be incremented in units of ten to achieve the
  # maxurls.
  strt <- 0
  related_sites <- vector('character', length = 0)
  
  while(length(related_sites) < maxurls) {
    # construct the google_query for related sites
    google_query <-
      sprintf("http://www.google.com/search?hl=en&lr=&ie=ISO-8859-1&q=related:%s&btnG=Search&ndplr=1&start=%d",
              site, strt)
    
    this_url_set <- get_href_urls(google_query)
    this_url_set <- this_url_set[!grepl(paste0(excludesites,collapse="|"),this_url_set)]
    
    related_sites <- c(related_sites, this_url_set)
    
    if (length(this_url_set) < 10L) {
      break
    } else {
      strt <- strt + 10
      Sys.sleep(delay)
    }
    
  } # end while loop
  
  related_sites[seq(1, maxurls, by = 1)]
}


get_href_urls <- function(site) {
  if (length(site) != 1L) {
    stop(sprintf("%s needs to be a single character string, a connection, or a raw vector.",
                 deparse(substitute(site))),
         call. = FALSE)
  }
  
  sites <-
    xml2::read_html(site) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  idx <-  grepl("^\\/url\\?q\\=", sites) &
    !grepl("webcache.googleusercontent.co", sites)
  
  gsub("\\/url\\?q=(.*)&sa.*$", "\\1", sites[idx])
}

