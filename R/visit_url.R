#' Visit Url
#'
#' Visit a Url and report social network analysis relevent details.
#'
#' @param x a url, specificied as a character string, or a html session
#' @param time_out time in seconds to set the timeout on how long to wait for a site to respond
#' @param ... not currently used.
#'
#' @return A \code{sna_visited_url} object.  This is a \code{data.frame} with
#' the following columns:
#' \describe{
#' \item{url}{the visited url}
#' \item{status}{the http status or error message status}
#' \item{access_date}{POSIXct date and time of the attempted visit}
#' \item{type}{content-type, if applicable}
#' \item{title}{title of the web page, if applicable}
#' }
#'
#' @export
visit_url <- function(x, time_out=10, ...) {
  UseMethod("visit_url")
}

#' @export
visit_url.session <- function(x, time_out=10, ...) {
  # visit_url(x$url, ...)

  content_type <- x$response$headers$`content-type`
  if(is.null(content_type)) content_type <- "unknown"
  
  out <-
    dplyr::data_frame(url         = x$url,
                      status      = as.character(x$response$status_code),
                      access_date = x$response$date,
                      type        = content_type)

  title <- try(rvest::html_text(rvest::html_nodes(x, "title")) %>% 
                 gsub("\\'|\\r|\\n|\\s{2,}","", .) %>% 
                 gsub('\\"','', .) %>% 
                 gsub(',',' ', .) %>% 
                 gsub('\\',' ', .,fixed=TRUE) %>% 
                 gsub('[^ -~]|[\x80-\xFF]|[^[:alnum:][:blank:]?&/\\-]','',.) %>% gsub("U00..",'',.)
               
            , silent = TRUE)
  out$title <- ifelse(inherits(title, "try-error"),  urltools::domain(out$url), title)
  # out$title <- ifelse(inherits(title, "try-error"), NA_character_, title)
  # if(!(nchar(out$title)>0)) out$title <- urltools::domain(out$url)
  out$url %<>%  gsub("\\/$", "", .) # omit trailing backslashs so that "../contact" and "../contact/" will be the same
  attr(out, "session") <- x
  class(out) <- c("sna_visited_url", class(out))
  out
}

#' @export
visit_url.character <- function(x, time_out=10, ...) {

  if (length(x) > 1L) {
    stop("length(x) > 1: you may only pass on url at a time to snaWeb::visit_url()", call. = FALSE)
  }

  this_session <- try(suppressWarnings(rvest::html_session(x,httr::timeout(time_out))),
                      silent = TRUE)

  if (inherits(this_session, "try-error")) {
    out <-
      dplyr::data_frame(url         = x,
                        status      = attr(this_session, "condition")$message,
                        access_date = as.POSIXct(as.POSIXlt(Sys.time(), tz = "GMT")),
                        type        = NA_character_,
                        title       = NA_character_)
    out$url %<>%  gsub("\\/$", "", .) # omit trailing backslashs so that "../contact" and "../contact/" will be the same
    attr(out, "session") <- this_session
    class(out) <- c("sna_visited_url", class(out))
  } else {
    cl <- as.list(match.call())[-1]
    cl$x <- this_session
    out <- do.call(visit_url, cl, ...) 
  }

  out
}
