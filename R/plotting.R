#' @title Plotting Social Networks
#'
#' @description Plotting tools for social network analysis
#'
#' @details The possible plotting methods are, initially, all provided by the
#' \pkg{igraph} package.  The methods, noted below, can be modified via passing
#' arguments through \code{...}.
#'
#' \describe{
#' \item{igraph}{uses \code{igraph:::plot.igraph}}
#' \item{tkplot}{uses \code{igraph::tkplot}}
#' \item{rglplot}{uses \code{igraph::rglplot}}
#' }
#'
#' @param x a \code{sna_urls} object
#' @param method the plotting method to use, defaults to basic R graphics via
#' \code{\link{igraph}{plot.igraph}}
#' @param ... other arguments passed to plotting method
#'
#' @seealso \code{\link{igraph}{plot.igraph}}
#'
#' @export
plot.sna_urls <- function(x, method = "igraph", ...) {

  cl <- match.call()
  cl <- as.list(cl)[-1]

  cl$x <- igraph::graph_from_data_frame(x$edges)

  switch(method,
         igraph  = do.call(igraph::plot.igraph, cl),
         tkplot  = do.call(igraph::tkplot, cl),
         rglplot = do.call(igraph::rglplot, cl),
         default = stop("Unknown ploting method.", call. = FALSE)) 
}
