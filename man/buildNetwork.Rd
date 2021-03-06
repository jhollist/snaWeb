% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildNetwork.R
\name{buildNetwork}
\alias{buildNetwork}
\title{Build Network}
\usage{
buildNetwork(sites = sites, searchtype = "related", snowball = FALSE,
  nodes = NULL, edges = NULL, excludesites = "none", delay = 1,
  maxurls = 10, max_depth = 5)
}
\arguments{
\item{sites}{is a vector or json of web sites for the search.}

\item{searchtype}{a vector. Type of links search. 'related','links' or 'both' (default is 'related').}

\item{snowball}{a boolean. If TRUE snowball search results (default is FALSE).}

\item{nodes}{a data frame. nodes to append to new search to (default is NULL).}

\item{edges}{a data frame. edges to append to new search to (default is NULL).}

\item{excludesites}{is a vector or json of web sites to be excluded from the search (default is \code{none})}

\item{delay}{(default is 0)}

\item{maxurls}{Maximum urls returned in a "related" search (default is 10)}

\item{max_depth}{Maximum depth a "linked" search will scrape}
}
\value{
What does this return?
}
\description{
The buildNetwork function runs a Google 'related' search for the input sites
or parses the site HTML for hyperlinked sites.
}
\examples{
\dontrun{
buildNetwork(sites='["http://npr.org"]',searchtype = "related")
}
}
