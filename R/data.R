utils::globalVariables(
    names = c("DO_colors", "DO_pubs", "ST_pubs", "obofoundry_metadata",
              "ns_prefix")
)

#' @keywords internal
pkg_user_agent <- "DO.utils (github.com/allenbaron/DO.utils)"


#' Prioritized List of Publication IDs for Matching
#'
#' @format A length-`r length(pub_id_types)` character vector:
#' \describe{ `r vctr_to_string(pub_id_types, delim = " > ")` }
pub_id_types <- c("pmid", "pmcid", "doi", "scopus_eid")


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


#' Publication Info
#'
#' Datasets describing official publications of the Human Disease Ontology
#' project, grouped by ontology. Data includes
#' publication identifiers from various sources, along with publication titles
#' and NLM-formatted citations.
#' * DO_pubs: Human Disease Ontology publications only
#' * ST_pubs: Symptom Ontology & Pathogen Transmission Ontology publications
#'
#' @format Each data frame consists of `r length(DO_pubs)` variables:
#' \describe{
#'   \item{internal_id}{short-version identifier used by the DO team}
#'   \item{pmid}{PubMed ID}
#'   \item{pmcid}{PubMed Central ID}
#'   \item{doi}{DOI}
#'   \item{scopus_eid}{Scopus Electronic Identification, not the same as a
#'   Scopus ID}
#'   \item{lens_id}{Lens.org ID}
#'   \item{semantic_scholar_id}{Semantic Scholar Corpus ID}
#'   \item{first_author}{Publication First Author}
#'   \item{title}{Publication Title}
#'   \item{citation_nlm}{Full NLM-formatted citation}
#' }
#'
#' @source Compiled by J. Allen Baron; last updated 2022-07-22.
#' @name pubs
NULL

#' @format `DO_pubs` includes `r nrow(DO_pubs)` publications.
#' @rdname pubs
"DO_pubs"

#' @format `ST_pubs` includes `r nrow(ST_pubs)` publications.
#' @rdname pubs
"ST_pubs"


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


#' Namespace Prefixes
#'
#' Prefixes for namespaces sourced from `robot` that cover most uses in
#' OBO Foundry ontologies, with a few additional prefixes and slight
#' modifications to match the prefixes as used in the Human Disease Ontology.
#'
#' @format A `r class(ns_prefix)` vector of `r length(ns_prefix)` namespaces,
#' named by their corresponding prefix
#' (e.g.`r paste0('\ua0', utils::tail(names(ns_prefix), 1), '\ua0=\ua0"', utils::tail(ns_prefix, 1), '"')`).
#'
#' @source "`robot` (v1.9.0) via data-raw/ns_prefix.R; last updated 2022-07-22."
"ns_prefix"
