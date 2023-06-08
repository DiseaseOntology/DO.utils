utils::globalVariables(
    names = c("DO_colors", "DO_pubs", "ST_pubs", "obofoundry_metadata",
              "ns_prefix")
)

#' @keywords internal
pkg_user_agent <- "DO.utils (github.com/DiseaseOntology/DO.utils)"


#' Prioritized List of Publication IDs & Regex's for Matching
#'
#' @format A length-`r length(pub_id_match)` character vector of named regex's
#' prioritized as follows:
#' \describe{ `r vctr_to_string(names(pub_id_match), delim = " > ")` }
pub_id_match <- c(
    pmid = "[0-9]{1,8}",
    pmcid = "PMC[0-9]+",
    doi = "10.+/.+",
    scopus_eid = "2-s2.0-[0-9]{11}"
)


#' Human Disease Ontology Official Colors
#'
#' Named, hexadecimal colors for official DO use.
#'
#' @format A named, vector of `r length(DO_colors)` colors in hexadecimal
#' format, including:
#' - main teal-ish colors: `r vctr_to_mancode(names(DO_colors)[1:5])`
#' - saturated versions (better for plotting): `r vctr_to_mancode(DO_colors, regex = "^sat", use_names = TRUE)`
#' - older, infrequently used orange colors: `r vctr_to_mancode(DO_colors, regex = "orange", use_names = TRUE)`
#' - _NEW_ lavender-ish accent colors: `r vctr_to_mancode(DO_colors, regex = "accent1", use_names = TRUE)`
#' - _NEW_ yellow-ish accent colors: `r vctr_to_mancode(DO_colors, regex = "accent2", use_names = TRUE)`
#'
#' @source Updated by J. Allen Baron on 2023-04-05.
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
#' Prefixes for namespaces, primarily sourced from
#' [ROBOT](http://robot.obolibrary.org/), that cover most uses in OBO Foundry
#' ontologies, with slight modification to match the prefixes as used in the
#' Human Disease Ontology and a few additional prefixes for cross-references and
#' DO-related SPARQL query support.
#'
#' @format
#' Character vectors of namespaces named by their corresponding prefix:
#'
#' `ns_prefix`: Superset of all `r length(ns_prefix)` objects listed below.
#'
#' @source `ROBOT` (v1.9.4) via data-raw/ns_prefix.R; last updated 2023-06-12.
"ns_prefix"

#' @format `obo_prefix`: `r length(obo_prefix)` _standard_ OBO Foundry
#' namespaces
#' (e.g.`r paste0('\ua0', utils::head(names(obo_prefix), 1), '\ua0=\ua0"', utils::head(obo_prefix, 1), '"')`).
#' @rdname ns_prefix
"obo_prefix"

#' @format `obo_prop_prefix`: `r length(obo_prop_prefix)` OBO Foundry _property_
#' namespaces; _these may not all be in actual use_
#' (e.g.`r paste0('\ua0', utils::head(names(obo_prop_prefix), 1), '\ua0=\ua0"', utils::head(obo_prop_prefix, 1), '"')`)
#' @rdname ns_prefix
"obo_prop_prefix"

#' @format `not_obo_prefix`: `r length(not_obo_prefix)` namespaces outside but
#' commonly used in the OBO Foundry
#' (e.g.`r paste0('\ua0', names(not_obo_prefix["dc"]), '\ua0=\ua0"', not_obo_prefix["dc"], '"')`),
#' or in federated SPARQL queries with the DO
#' (e.g.`r paste0('\ua0', names(not_obo_prefix["up"]), '\ua0=\ua0"', not_obo_prefix["up"], '"')`).
#'
#' @rdname ns_prefix
"not_obo_prefix"


# all possible extensions of OBO Foundry ontologies
ontology_ext <- c("OBO Graphs JSON" = "json", "OBO Format" = "obo",
                  "OWL Functional" = "ofn", "Manchester" = "omn",
                  "RDF/XML" = "owl", "OWL/XML" = "owx", "Turtle" = "ttl")
