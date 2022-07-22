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


to_range <- function(x, int_fn = NULL, ..., sep = c(",", "-"),
                     start_rm = NULL, end_rm = NULL) {
    uniq <- unique(x)
    if (!is.numeric(uniq) || any(!is_whole_number(uniq), na.rm = TRUE)) {
        if (is.null(int_fn)) {
            rlang::abort(
                message = "`int_fn` must be specified when `x` is not limited to whole numbers.")
        }

        int_fn <- rlang::as_function(int_fn)
        int <- int_fn(uniq, ...)
        if (!is.integer(int)) {
            rlang::abort(
                message = paste0(
                    "`int_fn` should produce an `integer` not `", class(int), "`"
                )
            )
        }

        uniq_order <- uniq[order(int)]
        int <- sort(int)
        in_seq <- c(0, diff(int)) == 1
    } else {
        uniq_order <- sort(as.integer(uniq))
        in_seq <- c(0, diff(uniq_order)) == 1
    }

    out_vctr <- NULL
    for (.i in seq_along(in_seq)) {
        if (is.na(in_seq[.i + 1])) {
            out_vctr <- c(
                out_vctr,
                if (!is.null(end_rm) && tail(out_vctr, 1) == "-") {
                    stringr::str_remove(uniq_order[.i], end_rm)
                } else {
                    uniq_order[.i]
                }
            )
        } else if (!in_seq[.i]) {
            start_seq <- in_seq[.i + 1] & in_seq[.i + 2]
            # Fix for error: 2-long range start at end
            if (is.na(start_seq)) start_seq <- FALSE

            sep_ <- if (start_seq) sep[2] else sep[1]
            uniq_order_ <- if (is.null(start_rm) || !start_seq) {
                uniq_order[.i]
            } else {
                stringr::str_remove(uniq_order[.i], start_rm)
            }
            out_vctr <- c(out_vctr, uniq_order_, sep_)
        } else if (!in_seq[.i + 1]) {
            out_vctr <- c(
                out_vctr,
                if (!is.null(end_rm) && tail(out_vctr, 1) == "-") {
                    stringr::str_remove(uniq_order[.i], end_rm)
                } else {
                    uniq_order[.i]
                },
                sep[1]
            )
        }
    }

    paste0(out_vctr, collapse = "")
}
