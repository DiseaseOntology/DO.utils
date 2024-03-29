% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ns_prefix}
\alias{ns_prefix}
\alias{obo_prefix}
\alias{obo_prop_prefix}
\alias{not_obo_prefix}
\title{Namespace Prefixes}
\format{
Character vectors of namespaces named by their corresponding prefix:

\code{ns_prefix}: Superset of all 533 objects listed below.

\code{obo_prefix}: 255 \emph{standard} OBO Foundry
namespaces
(e.g. AAO = "http://purl.obolibrary.org/obo/AAO_").

\code{obo_prop_prefix}: 255 OBO Foundry \emph{property}
namespaces; \emph{these may not all be in actual use}
(e.g. aao = "http://purl.obolibrary.org/obo/aao#")

\code{not_obo_prefix}: 21 namespaces outside but
commonly used in the OBO Foundry
(e.g. dc = "http://purl.org/dc/elements/1.1/"),
or in federated SPARQL queries with the DO
(e.g. up = "http://purl.uniprot.org/core/").
}
\source{
\code{ROBOT} (v1.9.4) via data-raw/ns_prefix.R; last updated 2023-06-12.
}
\usage{
ns_prefix

obo_prefix

obo_prop_prefix

not_obo_prefix
}
\description{
Prefixes for namespaces, primarily sourced from
\href{http://robot.obolibrary.org/}{ROBOT}, that cover most uses in OBO Foundry
ontologies, with slight modification to match the prefixes as used in the
Human Disease Ontology and a few additional prefixes for cross-references and
DO-related SPARQL query support.
}
\keyword{datasets}
