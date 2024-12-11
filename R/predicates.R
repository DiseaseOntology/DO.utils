#' Test if an Object is Invariant
#'
#' Test if an object is invariant (_i.e._ all values are equal, within a given
#' tolerance for numeric vectors).
#'
#' @param x object to be tested
#' @param na.rm logical indicating whether to exclude NA values
#' @param ... unused; for extensibility
#'
#' @family value predicates
#' @family predicates
#' @export
is_invariant <- function(x, ...) {
    UseMethod("is_invariant")
}

#' @export
#' @rdname is_invariant
is_invariant.default <- function(x, na.rm = FALSE, ...) {
    if (isTRUE(na.rm)) {
        x <- stats::na.omit(x)
    }
    length(unique(x)) == 1
}

#' @export
#' @rdname is_invariant
#' @param tol double, tolerance to use (for numeric vectors)
is_invariant.numeric <- function(x, na.rm = FALSE,
                                 tol = sqrt(.Machine$double.eps), ...) {
    if (isFALSE(na.rm)) {
        na_n <- sum(is.na(x))
        if (na_n == length(x)) return(TRUE)
        if (na_n > 0) return(FALSE)
    }

    diff(range(x, na.rm = na.rm)) < tol
}

#' @export
#' @rdname is_invariant
#' @param incl_nm Whether top-level names should be included in determining if a
#' list is invariant (default: `TRUE`).
is_invariant.list <- function(x, incl_nm = TRUE, ...) {
    nm <- names(x)
    if (isFALSE(incl_nm) || is.null(nm)) {
        return(length(unique(x)) == 1)
    }

    length(unique(x)) == 1 && length(unique(nm)) == 1
}

#' @export
#' @rdname is_invariant
is_invariant.data.frame <- function(x, ...) {
    nrow(unique(x)) == 1
}


#' Character value predicates
#'
#' These value predicates are designed to identify common values that appear
#' in character vectors.
#'
#' * `is_blank()` identifies "" or whitespace of any length
#'
#' * `is_missing()` identifies NA's and blanks
#'
#' @param x vector to be tested
#'
#' @family value predicates
#' @family predicates
#' @name char_val_predicates
NULL

#' @export
#' @rdname char_val_predicates
is_blank <- function(x) {
    assert_character(x)
    stringr::str_trim(x) == ""
}

#' @export
#' @rdname char_val_predicates
is_missing <- function(x) {
    is.na(x) | is_blank(x)
}


#' Number value predicates
#'
#' These value predicates are designed to identify common values that appear
#' in numeric vectors.
#'
#' `is_whole_number()` should generally be used when a whole number is
#' desired (whether integer or double) instead of [base::is.integer] or the
#' [rlang::is_integer] family because those test the data type no the value.
#'
#' @inheritParams char_val_predicates
#' @param tol value specifiying precision desired (see [.Machine] or [double]
#' for more info)
#'
#' @family value predicates
#' @family predicates
#' @name num_val_predicates
NULL

#' @export
#' @rdname num_val_predicates
is_positive <- function(x) {
    assert_numeric(x)
    x > 0 & is.finite(x)
}

#' @export
#' @rdname num_val_predicates
is_negative <- function(x) {
    assert_numeric(x)
    x < 0 & is.finite(x)
}

#' @export
#' @rdname num_val_predicates
is_whole_number <- function(x, tol = .Machine$double.eps)  {
    assert_numeric(x)
    abs(x - round(x)) < tol
}

#' @export
#' @rdname num_val_predicates
is_scalar_whole_number <- function(x, tol = .Machine$double.eps)  {
    rlang::is_scalar_atomic(x) && is_whole_number(x, tol = tol)
}


#' Logical predicate
#'
#' This predicate is designed to identify boolean vectors (i.e. length 1 logical
#' vectors).
#'
#' @inheritParams char_val_predicates
#'
#' @family type_predicates
#' @family predicates
#' @name lgl_predicates
NULL

#' @export
#' @rdname lgl_predicates
is_boolean <- function(x) {
    is.logical(x) & length(x) == 1
}


#' OBO ID predicates
#'
#' These predicates are designed to identify and validate common ID formats
#' defined within OBO Foundry ontologies.
#'
#' * `is_valid_obo()` to determine if identifiers match the OBO Foundry IRI
#' pattern or are an obo:LUI CURIE.
#'
#' * `is_valid_doid()` to determine if identifiers match DO's IRI or CURIE
#' pattern.
#'
#' @section Notes:
#' These predicates _do not_ attempt to confirm any ID actually exists in an
#' OBO Foundry ontology, but only test if the IDs are syntactically formatted
#' correctly (see [OBO Foundry ID Policy](https://obofoundry.org/id-policy) and
#' [OBO File Specification](https://owlcollab.github.io/oboformat/doc/obo-syntax.html))
#' _AND_ correspond to a known namespace of an OBO Foundry ontology (as provided
#' by ROBOT [export-prefixes](https://robot.obolibrary.org/export-prefixes).
#' These prefixes _should_ be up-to-date with the latest OBO Foundry ontologies
#' (https://github.com/ontodev/robot/issues/51).
#'
#' Not all OBO formats are valid DOID formats, but all valid DOID formats _ARE_
#' valid OBO formats.
#'
#' @param x A set of IDs, as a character vector.
#'
#' @examples
#' # OBO formats
#' obo_id <- c(
#'     #### valid ####
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "<http://purl.obolibrary.org/obo/CL_0000066>", # bracketed_URI
#'     "obo:DOID_4", # CURIE, standard version
#'     #### valid OBO property ####
#'     "obo:so#has_origin", # '#' separator ~ OBO annotation properties
#'     #### invalid ####
#'     "0001816", # bare number without prefix
#'     "obo:DOID:14566", # namespace-lui separator must be '_' ('#' for properties)
#'     " obo:HP_0000001" # must have NO `[:space:]` characters
#' )
#'
#' is_valid_obo(obo_id)
#'
#' # DOID formats
#' doid <- c(
#'     #### valid ####
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "DOID:4", # CURIE, standard version
#'     "obo:DOID_4", # OBO CURIE, less common
#'     "DOID_0040001", # basename (OBO prefix removed)
#'     #### invalid ####
#'     "0001816", # bare number without prefix
#'     "obo:DOID_21 ", # must have NO `[:space:]` characters
#'     # properties are NOT recognized as valid
#'     "doid:DO_IEDB_slim",
#'     "obo:doid#DO_IEDB_slim"
#' )
#'
#' is_valid_doid(doid)
#'
#' @family ID predicates
#' @family predicates
#' @name obo_ID_predicates
NULL

#' @rdname obo_ID_predicates
#' @export
is_valid_obo <- function(x) {
    assert_character(x)
    # regular expression components
    ns <- paste0("(", paste0(names(obo_prefix), collapse = "|"), ")")
    lui <- "[0-9]+"
    obo_lui <- paste0(ns, "_", lui)

    # full regular expressions for different formats
    uri <- paste0("^<?http://purl.obolibrary.org/obo/", obo_lui, ">?$")
    obo_curie <- paste0("^obo:", obo_lui, "$")
    onto_curie <- paste0("^", ns, ":", lui, "$")

    # final regex
    obo_regex <- paste(uri, obo_curie, onto_curie, sep = "|")

    stringr::str_detect(x, obo_regex)
}

#' @rdname obo_ID_predicates
#' @export
is_valid_obo_prop <- function(x) {
    assert_character(x)
    # regular expression components
    ns <- paste0("(", paste0(names(obo_prop_prefix), collapse = "|"), ")")
    lui <- "[[:alnum:]_]+"
    obo_lui <- paste0(ns, "#", lui)

    # full regular expressions for different formats
    uri <- paste0("^<?http://purl.obolibrary.org/obo/", obo_lui, ">?$")
    obo_curie <- paste0("^obo:", obo_lui, "$")
    onto_curie <- paste0("^", ns, ":", lui, "$")

    # final regex
    obo_regex <- paste(uri, obo_curie, onto_curie, sep = "|")

    stringr::str_detect(x, obo_regex)
}

#' @param strict Whether to enforce the DOID format strictly, as a boolean. If
#' `FALSE` (default), the `DOID_4` format without the `obo:` prefix will be
#' allowed.
#' @rdname obo_ID_predicates
#' @export
is_valid_doid <- function(x, strict = FALSE) {
    assert_character(x)
    obo_prefix <- "^(http://purl.obolibrary.org/obo/|obo:)"
    if (!strict) obo_prefix <- paste0(obo_prefix, "?")

    doid_regex <- paste0(obo_prefix, "DOID_[0-9]{1,7}$|^DOID:[0-9]{1,7}$")
    stringr::str_detect(x, doid_regex)
}


#' CURIE (RDF ID) Predicate
#'
#' This predicate is designed to validate CURIEs, compact URIs conforming to the
#' [W3C CURIE Syntax 1.0](https://www.w3.org/TR/2010/NOTE-curie-20101216/)
#' standard (or stricter as allowed by that standard).
#'
#' @param x A set of CURIEs, as a character vector.
#' @param def The definition of CURIEs to test against; as a string. One of
#'     `"obo"`, `"obo_generic"` (default), `"w3c"`, or `"w3c_safe"` (see `Notes`
#'     section for details).
#'
#' @section Notes:
#' The [W3C CURIE Syntax 1.0](https://www.w3.org/TR/2010/NOTE-curie-20101216/)
#' standard defines the most general CURIE syntax allowed (`def = "w3c"`; not a
#' perfect implementation, e.g. U+3000 non-breaking space is not accepted). Note
#' that by definition URIs will be identified as CURIEs. If it is desirable to
#' distinguish these use `def = "w3c_safe"` and wrap CURIEs in brackets (as
#' defined in the standard. Alternatively, use the stricter OBO Foundry-based
#' standards (as stated in that standard, more strict definitions for CURIEs can
#' be defined).
#'
#' `"obo"` corresponds to the official OBO Foundry definition of a CURIE as
#' stated in the [OBO Foundry ID Policy](http://obofoundry.org/id-policy.html).
#' This should work for _most_ class CURIEs from OBO Foundry ontologies but will
#' not recognize `obo:IDSPACE_LOCALID` formatted CURIEs or CURIEs with letters
#' or symbols in their local unique identifier. To allow these in the set while
#' still restricting to CURIE patterns found in OBO Foundry ontologies, use
#' `"obo_generic"`.
#'
#' @examples
#' id <- c(
#'     #### pass all ####
#'     "DOID:0001816", "CL:0000066",
#'     #### pass "obo_generic" & "w3c" ####
#'     "obo:DOID_4", "obo:so#has_origin", # obo prefixed CURIEs
#'     "oboInOwl:hasDbXref", "skos:exactMatch", # obo object properties
#'     "alfred:LO362836C", # not OBO but conforms to `"obo_generic"` pattern
#'     #### pass only "w3c" ####
#'     "4dn.biosource:4DNSR73BT2A2", "aceview.worm:aap-1",
#'     #### always fail ####
#'     "0001816", # bare number without prefix
#'     " obo:HP_0000001", # must have NO `[:space:]` characters
#'     "http://purl.obolibrary.org/obo/DOID_0001816" # URI
#' )
#'
#' @family ID predicates
#' @family predicates
#' @export
is_curie <- function(x, def = "obo_generic") {
    def <- match.arg(def, choices = c("obo", "obo_generic", "w3c", "w3c_safe"))

    if (def == "obo") pattern <- "^[A-Za-z_]+:[[:digit:]]+$"
    if (def == "obo_generic") pattern <- "^[A-Za-z_]+:[[:alnum:]#_]+$"
    if (def == "w3c") pattern <- "^[[:alnum:].-_]+:[[:graph:]]+$"
    if (def == "w3c_safe") pattern <- "^\\[[[:alnum:].-_]+:[[:graph:]]+\\]$"

    stringr::str_detect(x, pattern) & !stringr::str_detect(x, "^_:")
}

#' Test for All Values
#'
#' Returns `TRUE` if, and only if, all `values` are present in `x` and ONLY
#' those `values` are present in `x`.
#'
#' @param x A vector.
#' @param values The values to ensure exist in `x`.
#'
#' @returns
#' `TRUE` or `FALSE`. When `FALSE`, `missing` and/or `extra` attributes will be
#' included to assist in identifying non-conformity.
#'
#' @export
iff_all_vals <- function(x, values) {
    vals_present <- values %in% x
    only_vals <- all(x %in% values)

    out <- all(vals_present) && all(only_vals)
    if (any(!vals_present)) attr(out, "missing") <- values[!vals_present]
    if (any(!only_vals)) attr(out, "extra") <- x[!only_vals]

    out
}


#' Test for Hexadecimal Colors
#'
#' Tests whether values in a character vector are valid hexadecimal color codes.
#' Will _NOT_ recognize [abbreviated hex codes](https://en.wikipedia.org/wiki/Web_colors#Shorthand_hexadecimal_form)
#' (e.g. `#fff`).
#'
#' @param x A character vector.
#'
#' @family predicates
#' @export
is_hex_color <- function(x) {
    stringr::str_detect(x, "^#([0-9a-fA-F]{2}){3,4}$")
}


# INTERNAL Type tests --------------------------------------------------------

is_vctr_or_df <- function(x) {
    is.vector(x) || is.data.frame(x)
}

is_DOrepo <- function(x) {
    class(x)[1] == "pyDOID.repo.DOrepo"
}

is_owl_xml <- function(x) {
    class(x)[1] == "pyDOID.owl.xml"
}
