#' Get hrefs
#'
#' Given a url, or html session, find the absolute urls of relative and external
#' links posted on the web page.
#'
#' There are a few options for filtering the set of returned links:
#' \code{keep_regex}, \code{omit_regex}, and \code{omit_bookmarks}.  The first two
#' are regular expressions and will be applied to the set of links in order of
#' keep, then omit, that is: given a character vector of \code{links}, the use
#' of the \code{keep_regex} and \code{omit_regex} is equivalent to the following
#' two lines of code:
#'
#' \verb{> links <- links[grepl(keep_regex, links)]}
#'
#' \verb{> links <- links[!grepl(omit_regex, links)]}
#'
#' Both \code{keep_regex} and \code{omit_regex} are optional.  You may consider
#' runing \code{get_hrefs} without filting results and inspect the returned
#' urls.  Post hoc filter would be viable, as would re-evaluating the
#' \code{get_hrefs} call with the wanted filters.
#'
#' By default urls with the '#' symbol are omitted.  Set \code{omit_bookmarks =
#' FALSE} to include url with bookmarks in the return.
#'
#' @param x Either a character string of website of interest, or a
#' session has defined from the \pkg{rvest} function
#' \code{\link[rvest]{html_session}}
#' @param keep_regex a regular expression to be matched in the found hrefs, see
#' details.
#' @param omit_regex a regular expression to be matched in the found hrefs, see
#' details.
#' @param omit_bookmarks urls containing the "#" symbol will be omited from the
#' returned urls (Logical, defaults to \code{TRUE})
#' @param ... not currently used
#'
#' @return A \code{sna_hrefs} object, which is a \code{data.frame} with the
#' following columns:
#' \describe{
#' \item{url}{<chr> the found urls, modified to be absolute urls}
#' \item{relative}{<logical> indicates whether or not the url relative to the domain of \code{x}}
#' }
#' 
#' The return object as additional attributes
#' \describe{
#' \item{session}{<session> the html session}
#' }
#'
#' If the url or session does not resolve, the retruned \code{data.frame} will
#' have the aforementioned columns, but will have no rows.
#'
#' @seealso \code{vignette(topic = "snaWeb", package = "snaWeb")}
#'
#' @examples
#' \dontrun{
#' get_hrefs('neptuneinc.org')
#'
#' ## See the vignette for more details:
#' vignette("snaWeb", package = "snaWeb")
#' }
#'
#' @export
get_hrefs <- function(x, keep_regex = NULL, omit_regex = NULL, omit_bookmarks = TRUE, ...) {
  UseMethod("get_hrefs")
}

#' @export
get_hrefs.character <- function(x, keep_regex = NULL, omit_regex = NULL, omit_bookmarks = TRUE, ...) {
  if (length(x) > 1L) {
    stop("length(x) > 1: you may only pass on url at a time to snaWeb::get_hrefs()", call. = FALSE)
  }
  
  s <- try(suppressWarnings(rvest::html_session(x)), silent = TRUE)

  if (inherits(s, "try-error")) {
    out <- bad_url_return(s)
  } else if ( s$response$status_code == 404 || !grepl("html", s$response$headers$`content-type`)) {
    out <- bad_url_return(s)
  } else {
    cl <- as.list(match.call())[-1]
    cl$x <- s
    out <- do.call(get_hrefs, cl)
  } 
  out
}

#' @export
get_hrefs.session <- function(x, keep_regex = NULL, omit_regex = NULL, omit_bookmarks = TRUE, ...) {

  parent_url <- x$url
  links <-
    parent_url %>%
    {
      tryCatch(
        xml2::read_html(.),
        error=function(e) xml2::xml_new_root("html")
      )
    } %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    xml2::url_absolute(., parent_url) %>%
    gsub("\\/$", "", .) %>%  # omit trailing backslashs so that "../contact" and "../contact/" will be the same
    stats::na.omit(.) %>%
    unique(.) 
  
  if(length(links) == 0) {
    return(
      tibble::tibble(url      = character(),
                     relative = logical(),
                     child    = logical(),
                     sibling  = logical(),
                     parent   = logical()
      )
    )
  }
    
  if (!is.null(keep_regex)) {
    links <- links[grepl(keep_regex, links)]
  }

  if (!is.null(omit_regex)) {
    links <- links[!grepl(omit_regex, links)] 
  }

  # omit urls with a tag
  # if (omit_bookmarks) { 
  #   links <- links[!grepl("#", links)]
  # }

  # domain <- strsplit(parent_url, "/")[[1]][3]
  domain <- urltools::domain(parent_url)
  
  url_parsed   <- urltools::url_parse(parent_url)
  url_parsed   <- url_parsed %>% mutate(path = ifelse(is.na(path),"",path))
  links_parsed <- urltools::url_parse(links)
  
  # url_parse   <- unlist(strsplit(gsub("https://|http://","",parent_url),"/"))
  # links_parse <- strsplit(gsub("https://|http://","",links),"/")
  
  links_parsed <- 
    links_parsed %>% 
    mutate(url=links) %>%
    mutate(path = ifelse(is.na(path),"",path)) %>% 
    mutate(me = (scheme == url_parsed$scheme) & (domain == url_parsed$domain) & (path == url_parsed$path)) %>% 
    dplyr::mutate(relative = !me & domain == url_parsed$domain ) %>% 
    dplyr::mutate(child    = !me & relative & (is.na(url_parsed$path) | grepl(url_parsed$path,path))) %>%
    dplyr::mutate(sibling  = !me & relative & !grepl(url_parsed$path,path))
    # dplyr::mutate(parent   = 
    #   !me & !child &
    #   unlist(lapply(strsplit(path,"\\/"),function(frag){
    #     parent_path <- unlist(strsplit(url_parsed$path,"\\/"))
    #     parent_path <- parent_path[!grepl("htm",parent_path)]
    #     frag <- frag[!grepl("htm",frag)]
    #     any(parent_path %in% frag)
    #   })))
  
  class(links_parsed) <- c("sna_hrefs", class(links_parsed))
  attr(links_parsed, "session") <- x
  
  return(links_parsed)
  
  url_path   <- strsplit(url_parsed$path,"/")
  links_path <- strsplit(links_parsed$path,"/")
  url_path   <- unlist(lapply(url_path,function(frag){frag[!grepl("\\.",frag)]}))
  links_path <- lapply(links_path,function(frag){frag[!grepl("\\.",frag)]})
  
  relative   <- try(links_parsed$domain == url_parsed$domain)
  familytree <- try(lapply(links_path,function(link){link %in% url_path}))
  family     <- try(lapply(links_path,function(link){url_path %in% link}))
  child      <- try(unlist(lapply(family,function(fam){all(fam)})))
  if(!inherits(child,"try-error")){
    sibling    <- try(!child & unlist(lapply(familytree,function(fam){length(fam)==length(url_path) & all(fam[length(url_path)-1])})))
    parent     <- try(!child & unlist(lapply(familytree,function(fam){length(fam)<length(url_path) & all(fam[length(url_path)-1])})))
  }
  
  # if( inherits(relative,"try-error") | inherits(familytree,"try-error") | inherits(family,"try-error") | inherits(child,"try-error") |
  #     inherits(sibling,"try-error") | inherits(parent,"try-error") ) browser()
  
  out <-
    tibble::tibble(url = links,
                  relative = relative,
                  child = child,
                  sibling = sibling,
                  parent = parent
  )
  
  class(out) <- c("sna_hrefs", class(out))
  attr(out, "session") <- x
  out
}

bad_url_return <- function(s) {
  out <- dplyr::tibble(url = character(0), relative = logical(0))
  attr(out, "session") <- s
  class(out) <- c("sna_hrefs", class(out))
  out
}


