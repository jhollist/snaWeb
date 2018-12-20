#' Linked Sites
#'
#' Crawl a website, building a site map, and reporting all internal and external
#' links found.
#'
#' The \code{max_depth} controls the number of links to follow.  The root url is
#' level 0 and all the hrefs found on that page are level 1.  Each href on a
#' level 1 page are labeled level 2.  These labels and processing of the pages
#' will continue through level \code{max_depth}.  You could think of
#' \code{max_depth} as the number of mouse clicks needed to navagate a web page
#' by a human in a graphical web browser to the noted url or file.
#'
#' @param x The root url as a character string, or a html session.
#' @param delay number of seconds to delay between http requests.
#' @param max_depth Starting with the root url (level 0) follow links upto \code{max_depth} "clicks".
#' @param excludesites (default is "none"
#' @param ... additional arguments (not yet used)
#'
#' @export
linked_urls <- function(x, delay = 0.2, max_depth = 5, excludesites="none", ...) {
  UseMethod("linked_urls")
}
#' @export
linked_urls.character <- function(x, delay = delay, max_depth = max_depth, excludesites = excludesites, ...) {
  
  if( grep("\\.",x)==1 & !grepl("www",x) ){
    x <- gsub("://","://www.",x)
  }
  cl <- as.list(match.call())[-1]
  cl$x <- try( rvest::html_session(x,httr::timeout(5)), silent = TRUE )
  if( inherits(cl$x,"try-error") ) {
    out <-
      list(nodes      = data.frame(url=x,
                                   rooturl=gsub("http://|https://","",x),
                                   name= gsub("http://|https://","",x),
                                   is_root="TRUE",
                                   specification=paste0('{"is_root":"TRUE","url":"',x,'","name":"',x,'"}')
                                   ),
           edges      = NULL,
           message    = "Success",  # Hold over form older version... 
           is_blocked = FALSE)      # Hold over form older version... 
    class(out) <- c("sna_linked_urls", "sna_urls")
    return(out)
  }else{
    do.call(linked_urls, cl)
  }
}

#' @export
linked_urls.session <- function(x, delay = delay, max_depth = max_depth, excludesites = excludesites, ...) {
  # check that the max_depth is an integer valued and at least 1
  max_depth <- floor(max_depth)
  if (max_depth < 1L) {
    warning("max_depth is being set to 1.", call. = FALSE, immediate. = TRUE)
    max_depth <- 1L
  }

  root_url <- x$url
  root_domain <- urltools::domain(root_url)

  all_urls <- visit_url(root_url)

  all_urls$internal <- grepl(root_domain, urltools::domain(all_urls$url))

  all_urls %<>% dplyr::mutate(hrefs = list(get_hrefs(url, omit_regex = "^mailto")))
  all_urls$depth <- 0L

  current_depth <- 1L

  cat("max_depth:", max_depth,"\n")
  while (current_depth <= max_depth) {

    message(sprintf("current_depth: %d", current_depth))

    links_to_visit <-
      all_urls %>%
      dplyr::filter(.data$depth == current_depth - 1L) %>%
      magrittr::extract2("hrefs") %>%
      dplyr::bind_rows(.) %>%
      dplyr::distinct(.)

    message(sprintf("%d urls to visit", nrow(links_to_visit)))
    if (nrow(links_to_visit) > 0) {
      links_to_visit %<>% dplyr::anti_join(., all_urls, by = "url")
    } else {
      break
    }

    if (nrow(links_to_visit) == 0) {
      break
    }

    v_urls <-
      pbapply::pblapply(seq_along(links_to_visit$url),
        function(i) {
          u <- links_to_visit$url[i]
          r <- links_to_visit$relative[i]
          cat("\n     ---",u)
          v <- try(visit_url(u))  ## Add error handle here?  Other regex to omit from get_href?
          if (r && v$status == 200) {
            v$hrefs <- list(get_hrefs(attr(v, "session"), omit_regex = "^mailto"))
          }
          cat(" success\n")
          Sys.sleep(delay)
          v
        }) %>%
      dplyr::bind_rows()

    if (nrow(v_urls)) {
      all_urls %<>%
        dplyr::bind_rows(.,
                         dplyr::mutate(v_urls,
                                       internal = grepl(root_domain, urltools::domain(.data$url)),
                                       depth = current_depth)
                       )
    }
    current_depth <- current_depth + 1L
  }

  linked_sites <- all_urls %>% 
    dplyr::mutate(rooturl=urltools::domain(url)) %>% 
    dplyr::filter(!internal & status == "200" & !duplicated(rooturl) & !grepl(paste0(excludesites,collapse="|"),rooturl))

  nodes <-
    dplyr::data_frame(url = c(x$url, linked_sites$url),
                      rooturl = c(root_domain,linked_sites$rooturl),
                      name = c(get_page_title(x$url),linked_sites$title),
                      is_root = c("TRUE", rep("FALSE", nrow(linked_sites)))
    ) 
    # %>% dplyr::mutate(id = seq_along(.data$url))
  
  nodes$name[is.na(nodes$name)] <- nodes$rooturl[is.na(nodes$name)]
  nodes$name[nchar(nodes$name)==0] <- nodes$rooturl[nchar(nodes$name)==0]
  nodes$name <- gsub("http://|https://","",nodes$name)
  
  nodes$specification <-
    with(nodes, sprintf('{"is_root":"%s","url":"%s","name":"%s"}', is_root, url, name))

  # build the data frame to define the edges of the social network graph.
  # edges <-
  #   dplyr::data_frame(name_from = x$url,
  #                     name_to   = linked_sites$url,
  #                     id = seq_along(.data$name_to)
  #   ) %>%
  #   dplyr::left_join(.,
  #                    dplyr::select(nodes, .data$url, node_to = .data$id),
  #                    by = c("name_to" = "url")) %>%
  #   dplyr::left_join(.,
  #                    dplyr::select(nodes, .data$url, node_from = .data$id),
  #                    by = c("name_from" = "url")) %>%
  #   dplyr::mutate(rank = .data$node_to - .data$node_from)
  # 
  # edges$predicate <-
  #   with(edges, sprintf('{"predicate":"is related","rank":"%d"}', rank))

  if( length(linked_sites$rooturl) > 0 ) {
    edges <-
      dplyr::data_frame(name_from = root_domain,
                        name_to   = linked_sites$rooturl)
  
    edges$predicate <-
      with(edges, sprintf('{"predicate":"is linked","rank":"NA"}'))
  
  }else{
    edges <- NULL
  }
  # return object
  out <-
    list(nodes      = nodes,
         edges      = edges,
         message    = "Success",  # Hold over form older version... 
         is_blocked = FALSE)      # Hold over form older version... 
  

  class(out) <- c("sna_linked_urls", "sna_urls")
  
  return(out)
}
