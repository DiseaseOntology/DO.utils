utils::globalVariables(names = c("DO_pubs", "obofoundry_metadata", "DO_colors"))

#' Human Disease Ontology (DO) Publication Info
#'
#' A dataset of identifiers for official DO publications, along with their
#' title and NLM-formatted citations.
#'
#' @format A data frame with 9 variables and 1 row for each official DO
#'     publication:
#' \describe{
#'   \item{internal_id}{short-version identifier used by the DO team for DO
#'   publications}
#'   \item{pmid}{PubMed ID}
#'   \item{pmcid}{PubMed Central ID}
#'   \item{doi}{DOI}
#'   \item{scopus_eid}{Scopus Electronic Identification, not the same as a
#'   Scopus ID}
#'   \item{semantic_scholar_id}{Semantic Scholar Corpus ID}
#'   \item{first_author}{Publication First Author}
#'   \item{title}{Publication Title}
#'   \item{citation_nlm}{full NLM-formatted citation}
#' }
#'
#' @source Compiled by J. Allen Baron; last updated 2021-11-11
"DO_pubs"

#' OBO Foundry Metadata
#'
#' Metadata about OBO Foundry ontologies.
#'
#' @format A data frame with 22 variables:
#' \describe{
#'   \item{id}{}
#'   \item{title}{}
#'   \item{domain}{}
#'   \item{description}{}
#'   \item{activity_status}{}
#'   \item{is_obsolete}{}
#'   \item{replaced_by}{}
#'   \item{ontology_purl}{}
#'   \item{preferred_prefix}{}
#'   \item{homepage}{}
#'   \item{contact_name}{}
#'   \item{contact_email}{}
#'   \item{contact_github}{}
#'   \item{license_label}{}
#'   \item{taxon_id}{}
#'   \item{taxon_label}{}
#'   \item{twitter}{}
#'   \item{facebook}{}
#'   \item{publications}{}
#'   \item{in_foundry_order}{}
#'   \item{in_foundry}{}
#'   \item{build_infallible}{}
#' }
#'
#' @source \url{http://www.obofoundry.org/registry/ontologies.jsonld}, last accessed 2021-11-05.
"obofoundry_metadata"


#' Human Disease Ontology Official Colors
#'
#' Named, hexadecimal colors for official DO use.
#'
#' @format A named, vector of colors in hexadecimal format, including most
#' recognizable teal-ish colors (default, mid, light, website, & websafe) and
#' less used oranges (orange, orange_mid, orange_light).
#'
#' @source Updated by J. Allen Baron on 2021-11-23.
"DO_colors"
