% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_pmid.elink_list}
\alias{extract_pmid.elink_list}
\title{Extract PubMed ID from elink_list object}
\usage{
\method{extract_pmid}{elink_list}(x, no_result = "warning", ...)
}
\arguments{
\item{x}{An \code{elink_list} object, generated by \link[rentrez:entrez_link]{rentrez::entrez_link} and it's
derivatives (e.g. \link{citedby_pmid}) when \code{by_id = TRUE} .}

\item{no_result}{The type of condition that should be signaled when no PubMed
results exist in a response; one of "error", "warning" (default),
"message" or "none".}

\item{...}{Additional arguments passed on to \code{\link[=extract_pmid.elink]{extract_pmid.elink()}}.}
}
\description{
Extract PubMed ID from an \code{elink_list} object.
}
\seealso{
Other extract_pmid documentation: 
\code{\link{extract_pmid}()},
\code{\link{extract_pmid.elink}()}
}
\concept{extract_pmid documentation}
