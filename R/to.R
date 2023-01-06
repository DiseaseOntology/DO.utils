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
        stats::setNames(paste0(names(ns_prefix), ":"), ns_prefix)
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
        stats::setNames(ns_prefix, paste0("^", names(ns_prefix), ":"))
    )
}

#' Convert Vectors to Range String
#'
#' Converts vectors to a string of ranges. All vector types are accepted and the
#' original values will appear in the final output range, but where input is not
#' a numeric vector of [whole numbers](is_whole_number()), a function to convert
#' the values to integers must be provided (`int_fn`) for the purpose of
#' identifying the range(s).
#'
#' @param x A numeric vector of [whole numbers](is_whole_number()).
#' @param int_fn A function (or tidyverse-style formula) to convert `x` into an
#' integer vector, used _ONLY for creating ranges_; the original value will
#' appear in the range except where modified by `start_rm` and/or `end_rm`.
#'  `int_fun` is required when `x` is not a numeric vector.
#' @param ... Arguments passed on to `int_fn`.
#' @param sep The separators to use between ranges (default: ',') and within
#' a range (default: '-'), as a length-2 character vector.
#' @param start_rm A regular expression to remove from `x` values at the
#' _beginning_ of a range.
#' @param end_rm A regular expression to remove from `x` values at the
#' _end_ of a range.
#'
#' @returns
#' The range(s) formatted as a string or `NA` if the input is an empty vector.
#'
#' @section Notes:
#'
#' * `NA` values are always dropped.
#'
#' * `to_range()` was inspired by answers at
#' https://stackoverflow.com/q/16911773/6938922, most heavily by speendo
#' (CC-BY-SA 3.0, accessed 2022-07-01). This is the fastest approach with
#' few inputs but is significantly slower than other answers for large inputs.
#' The internal approach will likely be modified in the future, but arguments
#' and output will remain the same.
#'
#' @examples
#' x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)
#' to_range(x)
#'
#'
#' # `NA` values are dropped
#' y <- c(1:4, NA, 5, 7:10)
#' to_range(x)
#'
#'
#' # Use `int_fn` when `x` is not a numeric vector (tidyverse-style formulas
#' #    accepted)
#' x_char <- as.character(x)
#' to_range(x, int_fn = as.integer)
#' to_range(x, int_fn = ~ as.integer(.x))
#'
#'
#' # `int_fn` allows non-numeric ranges to be created
#' txt <- paste0(x, "txt")
#' to_int <- function(x, y) as.integer(stringr::str_remove(x, "txt"))
#' to_range(txt, to_int, y = "txt")
#'
#'
#' # text can be selectively removed from the values at the beginning of ranges
#' #     (`start_rm`) or end of ranges (`end_rm`)
#' to_range(txt, to_int, start_rm = "txt")
#' to_range(txt, to_int, end_rm = "txt")
#'
#' @export
to_range <- function(x, int_fn = NULL, ..., sep = c(",", "-"),
                     start_rm = NULL, end_rm = NULL) {
    if (length(x) == 0) {
        return(NA)
    }

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
                if (!is.null(end_rm) && utils::tail(out_vctr, 1) == "-") {
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
                if (!is.null(end_rm) && utils::tail(out_vctr, 1) == "-") {
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
