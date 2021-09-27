#' Human Disease Ontology (DO) Publication Info
#'
#' A dataset of identifiers for official DO publications, along with their
#' title and NLM-formatted citations.
#'
#' @format A data frame with 8 rows and 8 variables:
#' \describe{
#'   \item{internal_id}{short-version identifier used by the DO team}
#'   \item{pmid}{PubMed ID}
#'   \item{pmcid}{PubMed Central ID}
#'   \item{doi}{DOI}
#'   \item{scopus_eid}{Scopus Electronic Identification}
#'   \item{semantic_scholar_id}{Semantic Scholar Corpus ID}
#'   \item{title}{Publication Title}
#'   \item{citation_nlm}{full NLM-formatted citation}
#' }
#'
#' @source Compiled by J. Allen Baron; last updated 2021-09-27
"DO_pubs"
