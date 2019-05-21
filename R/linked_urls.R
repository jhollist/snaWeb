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
#' @param excludesites (default is "none")
#' @param time_out time in seconds to set the timeout on how long to wait for a site to respond
#' @param keep_internal a boolean indicating whether to keep all internal pages the site 
#' @param keep_subpages a boolean indicating whether to keep all sub pages pages the site
#' @param ... additional arguments (not yet used)
#'
#' @export
linked_urls <- function(x, delay = 0.2, max_depth = 5, excludesites="none", time_out=10, keep_internal=FALSE, keep_subpages=FALSE, ...) {
  UseMethod("linked_urls")
}
#' @export
linked_urls.character <- function(x, delay = 0.2, max_depth = 5, excludesites="none", time_out=10, keep_internal=FALSE, keep_subpages=FALSE, ...) { #delay = delay, max_depth = max_depth, excludesites = excludesites, ...) {
  requireNamespace("magrittr")

  if( grep("\\.",x)==1 & !grepl("www",x) & length(unlist(strsplit(x,"\\.")))<3 ){
    x <- gsub("://","://www.",x)
  }
  cl <- as.list(match.call())[-1]
  cl$x <- try( rvest::html_session(x,httr::timeout(time_out)), silent = TRUE )
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
    do.call(linked_urls, cl,...)
  }
}

#' @export
linked_urls.session <- function(x, delay = 0.2, max_depth = 5, excludesites="none", time_out=10, keep_internal=FALSE, keep_subpages=FALSE,...) { # delay = delay, max_depth = max_depth, excludesites = excludesites, ...) {
  # check that the max_depth is an integer valued and at least 1
  requireNamespace("magrittr")
  max_depth <- floor(max_depth)
  if (max_depth < 1L) {
    warning("max_depth is being set to 1.", call. = FALSE, immediate. = TRUE)
    max_depth <- 1L
  }

  root_url    <- gsub("/$","",x$url)
  root_domain <- urltools::domain(root_url)

  omit_regex <- paste0(paste(excludesites,collapse="|"),"|^mailto|pdf$|jpg$|png$|ppt$|pptx$|xls$|xlsx$|doc$|docx$|mp4$|mov$|avi$|flv$|wmv$")
  # omit_regex <- paste0(paste(excludesites,collapse="|"),"|",gsub("www.","",root_domain),"|^mailto|pdf$|jpg$|png$|ppt$|pptx$|xls$|xlsx$|doc$|docx$")
  
  all_urls <- visit_url(root_url,time_out=time_out)
  #all_urls <- visit_url(root_url,time_out=time_out)

  all_urls$internal <- grepl(root_domain, urltools::domain(all_urls$url))
  all_urls$subpage  <- try(grepl(root_url, all_urls$url) & (root_url != all_urls$url))

  if(inherits(all_urls$subpage,"try-error")) browser()
  
  # all_urls$internal <- grepl(root_url, urltools::domain(all_urls$url))

  all_urls <- all_urls %>% dplyr::mutate(hrefs = list(get_hrefs(url,omit_regex=omit_regex )))

  all_urls$depth <- 0L
  
  current_depth <- 1L
  
  cat("max_depth:", max_depth," root_url: ",root_url," root_url:" ,root_url,"\n")
  while (current_depth <= max_depth) {

    message(sprintf("current_depth: %d", current_depth))

    links_to_visit <-
      all_urls %>%
      # dplyr::filter((root_url==.data$url)) %>%
      dplyr::filter(grepl("text",.data$type)) %>%
      dplyr::filter(.data$depth == current_depth - 1L) %>%
      magrittr::extract2("hrefs") %>%
      dplyr::bind_rows(.) %>%
      dplyr::distinct(.) %>%
      dplyr::filter(!((root_domain == gsub("http://|https://","",.data$url) )) )
    # dplyr::filter(!parent) %>% 
      # dplyr::filter(!sibling)
    
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
        function(i) { # i = 1
          u <- links_to_visit$url[i]
          r <- links_to_visit$relative[i] 
          c <- links_to_visit$child[i] 
          cat("\n     ---",u)
          v <- try(visit_url(u, time_out=time_out))  ## Add error handle here?  Other regex to omit from get_href?
          if (c && v$status == 200) {
            v$hrefs <- list(snaWeb::get_hrefs(attr(v, "session"), omit_regex = omit_regex))
            cat("  success")
          }
          cat("  \n")
          
          # if( inherits(v, "try-error") ){
          #   if (r && v$status == 200) {
          #     v$hrefs <- list(snaWeb::get_hrefs(attr(v, "session"), omit_regex = omit_regex))
          #     cat(" success\n")
          #   }else{
          #     cat(" fail\n")
          #   }
          # }else{
          #   cat(" fail\n")
          # }
          Sys.sleep(delay)
          v
        }) %>%
      dplyr::bind_rows()

    if (nrow(v_urls)) {
      all_urls %<>%
        dplyr::bind_rows(.,
                         dplyr::mutate(v_urls,
                                       internal = grepl(root_domain, urltools::domain(.data$url)),
                                       subpage = grepl(root_url, .data$url) & (root_url != .data$url),
                                       depth = current_depth)
                       )
    }
    current_depth <- current_depth + 1L
  }

  linked_sites <- all_urls %>% 
    dplyr::mutate(id=seq_along(.data$url)+(.data$depth*10)) %>% 
    dplyr::mutate(rooturl=urltools::domain(url)) %>% 
    dplyr::filter((!internal | keep_internal | depth==0) &
                  (!subpage | (keep_subpages & keep_internal)) &
                  (status == "200") & 
                  !duplicated(url) & 
                  # !duplicated(rooturl) & 
                  grepl("text",type) &
                  !grepl(paste0(excludesites,collapse="|"),rooturl))
  
  nodes <-
    dplyr::tibble(
      id = linked_sites$id,
      url = linked_sites$url,
      rooturl = linked_sites$rooturl,
      name = linked_sites$title,
      is_root = linked_sites$depth==0
  )
  # nodes <-
  #   dplyr::data_frame(
  #     id = c(1,linked_sites$id),
  #     url = c(root_url, linked_sites$url),
  #     # url = c(x$url, linked_sites$url),
  #     rooturl = c(root_domain,linked_sites$rooturl),
  #     name = c(snaWeb::get_page_title(root_url,timeout=time_out),linked_sites$title),
  #     # name = c(snaWeb::get_page_title(x$url,timeout=time_out),linked_sites$title),
  #     is_root = c("TRUE", rep("FALSE", nrow(linked_sites)))
  #   ) 
    # %>% dplyr::mutate(id = seq_along(.data$url))

#  Error in bind_rows_(x, .id) : Argument 1 must have names [1] "Linked Error: site http://www.ny4p.org excludesites [\"none\"] Error in bind_rows_(x, .id) : Argument 1 must have names\n  
  
  #if(inherits(try(nchar(nodes$name)),"try-error")) browser()
  # gsub('[^ -~]', [\x80-\xFF]   [^[:alnum:][:blank:]?&/\\-] then "U00.."
  
  nodes$name[is.na(nodes$name)] <- nodes$rooturl[is.na(nodes$name)]
  if( !inherits(try(nchar(nodes$name)),"try-error") ){
    nodes$name[nchar(nodes$name)==0] <- nodes$rooturl[nchar(nodes$name)==0]
  }
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
      dplyr::tibble(name_from = rep(nodes$url[nodes$is_root],nrow(linked_sites)-1),
                        name_to   = nodes$url[!nodes$is_root],
                        node_from = rep(nodes$id[nodes$is_root],nrow(linked_sites)-1),
                        node_to   = nodes$id[!nodes$is_root]
                        )
    
    
    
      # dplyr::data_frame(name_from = root_url,
      #                   name_to   = linked_sites$url)
    # dplyr::data_frame(name_from = root_domain,
    #                   name_to   = linked_sites$rooturl)
    
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
         is_blocked = FALSE,      # Hold over form older version... 
         depth = current_depth
  )
  

  # class(out) <- c("sna_linked_urls", "sna_urls")
  
  return(out)
}
