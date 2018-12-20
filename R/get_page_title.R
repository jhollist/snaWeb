#' Get Page Title
#'
#' Get a page title
#' @param site url as a character string, or a html session.
#' @param timeout A numeric specifying the maximum number of seconds the expression is allowed to run before being interrupted by the timeout.  
#' @param onTimeout A character specifying what action to take if a timeout event occurs
#'
#' @export
get_page_title <- function(site, timeout = 10, onTimeout = "error") { 
  # THERE IS NO NEED FOR A TIMEOUT HERE... html_session can control timeout
  
  pg <- R.utils::withTimeout(try(xml2::read_html(site), silent = TRUE),
                             timeout = timeout, onTimeout = onTimeout)
  
  if(grepl("questia",site)) browser()
  
  if (!inherits(pg, "try-error")) {
    pg_title <-
      pg %>%
      rvest::html_nodes("title") %>%
      rvest::html_text() %>%
      magrittr::extract(1) %>%
      gsub("\\'|\\t|\\r|\\n|\\s{2,}", "", .) %>%
      gsub('\\"','', .)
  } else {
    pg_title <- site
  }
  
  if( !(nchar(pg_title)>0) | is.na(pg_title) ) pg_title <- urltools::domain(site)
  if( !(nchar(pg_title)>0) ) pg_title <- site
  pg_title <- gsub("http://|https://","",pg_title)
  
  # if( (nchar(pg_title)>0) ){
  #   foo <- try(grep("Together We Survive",pg_title))
  #   if( inherits(foo, "try-error") ) browser()
  #   if( regexpr("Together We Survive",pg_title) > 0 ) {
  #     browser()
  #   }
  # }
  
  cat(pg_title,"\n")
  # if(grepl("Posts tagged",pg_title)) browser()
  
  pg_title
}