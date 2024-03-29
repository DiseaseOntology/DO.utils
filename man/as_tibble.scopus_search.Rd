% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_tibble-methods.R
\name{as_tibble.scopus_search}
\alias{as_tibble.scopus_search}
\alias{as_tibble.scopus_search_list}
\title{Convert \code{scopus_search} Object into Tibble}
\usage{
\method{as_tibble}{scopus_search}(x, ...)

\method{as_tibble}{scopus_search_list}(x, ...)
}
\arguments{
\item{x}{A \code{scopus_search} or \code{scopus_search_list} object.}

\item{...}{Ignored; included for extensibility.}
}
\value{
An untidy tibble. \code{scopus_search_list} objects will have an additional
\code{cites} column.
}
\description{
Converts a \code{scopus_search} object into a \link[tibble:tibble]{tibble}.
}
\seealso{
citedby_scopus
}
