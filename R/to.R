#' Convert URI to CURIE
#'
#' Converts URI(s) to CURIE(s).
#'
#' @param x URI(s), as a character vector.
#'
#' @section Note:
#' Performs no URI validation, relying on simple string matching of
#' namespace-prefix pairs of [ns_prefix] for conversion. Any values not
#' matching one of these will be returned without modification.
#'
#' @examples
#' .uri <- c(
#'     "http://www.w3.org/2000/01/rdf-schema#comment",
#'     "http://purl.org/dc/elements/1.1/date",
#'     "http://purl.org/dc/terms/license",
#'     "http://www.w3.org/2002/07/owl#deprecated",
#'     "http://www.geneontology.org/formats/oboInOwl#id",
#'     "http://purl.obolibrary.org/obo/UBERON_0000002",
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "http://purl.obolibrary.org/obo/doid#DO_AGR_slim"
#' )
#' to_curie(.uri)
#'
#' # uses 'obo' namespace when an OBO Foundry ontology namespace isn't available
#' to_curie(
#'     c("http://purl.obolibrary.org/obo/SO_0000110",
#'     "http://purl.obolibrary.org/obo/so#has_origin")
#' )
#'
#' #returns non-URI or unknown namespace prefixes unmodified
#' to_curie(
#'     c("http://purl.obolibrary.org/obo/SYMP_0000000",
#'     "not a URI", "https://disease-ontology.org/")
#' )
#'
#' @family identifier converters
#' @export
to_curie <- function(x) {
    stringr::str_replace_all(
        x,
        setNames(paste0(names(ns_prefix), ":"), ns_prefix)
    )
}


#' Convert CURIE to URI
#'
#' Converts CURIE(s) to URI(s).
#'
#' @param x CURIE(s), as a character vector.
#'
#' @section Note:
#' Performs no CURIE validation but relies on a properly formatted CURIE for
#' conversion (e.g. prefix:reference, according to the
#' [CURIE standard](https://www.w3.org/TR/2010/NOTE-curie-20101216/#s_syntax)).
#' Any inputs not matching a prefix of [ns_prefix] will be returned without
#' modification.
#'
#' @examples
#' .curie <- c("rdfs:comment", "dc:date", "terms:license", "owl:deprecated",
#'             "oboInOwl:id", "UBERON:0000002", "DOID:0001816",
#'             "doid:DO_AGR_slim")
#' to_uri(.curie)
#'
#' #returns non-CURIE or unknown namespace prefixes unmodified
#' to_curie(c("SYMP:0000000", "not a CURIE", "bioregistry.collection:0000001"))
#'
#' @family identifier converters
#' @export
to_uri <- function(x) {
    stringr::str_replace_all(
        x,
        setNames(ns_prefix, paste0("^", names(ns_prefix), ":"))
    )
}
