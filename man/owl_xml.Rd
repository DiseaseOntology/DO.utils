% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pyDOID.R
\name{owl_xml}
\alias{owl_xml}
\title{Instantiate an owl_xml object}
\usage{
owl_xml(path)
}
\arguments{
\item{path}{The path to an OWL (RDF) XML-formatted file, as a string.}
}
\description{
Instantiate a python \code{owl.xml} object from the
\href{https://github.com/allenbaron/pyDOID}{pyDOID} package (powered by
\href{https://github.com/rstudio/reticulate}{reticulate}). The file will be loaded
into memory when instantiated. Though named "owl" this method will also work
for RDF files.
}
\section{Classes & Methods}{

The \code{owl.xml} class thinly wraps a small set of python
\href{https://rdflib.readthedocs.io/en/stable/}{rdflib} methods to enable SPARQL
1.1 queries, with the following methods:
\itemize{
\item \code{owl_xml$query(query, reload)}: Execute a SPARQL 1.1 query. \code{query} may be
a string or the path to a .sparql/.rq file. Use \code{reload = TRUE} to force
reload a file into memory; useful when iterating through releases/tags with
\code{DOrepo$tag_iterate()}.
\item \code{owl_xml$load()}: Manually load a file into memory. Generally, this should
not be necessary.
}
}

\seealso{
Other pyDOID classes: 
\code{\link{DOrepo}()}
}
\concept{pyDOID classes}
