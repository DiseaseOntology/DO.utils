#' Identify all duplicates
#'
#' Built on [base::duplicated()] but, unlike `base::duplicated()`,
#' identifies all duplicates _including_ the first occurrence.
#'
#' @inheritParams base::duplicated
#'
#' @family predicates
#' @export
all_duplicated <- function (x, ...) {
    duplicated(x, ...) | duplicated(x, fromLast = TRUE, ...)
}


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
#' @param x vector to be tested
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
#' @param x vector to be tested
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
#' _AND_ correspond to a known namespace of an OBO Foundry ontology (see
#' [obo_prefix]).
#'
#' @param x A set of IDs, as a character vector.
#' @param format The OBO ID format(s) to validate against; as a character
#' vector. One or more of: `"standard"` (default; all except `"ns.lui"`),
#' `"curie"`, `"obo_curie"`, `"uri"`, `"<uri>"`, or `"ns.lui"` (i.e. an
#' OBO CURIE without `obo:`).
#' @param ns_type The type of OBO namespaces to validate against; as a character
#' vector. One of: `"any"` (default), `"ont"` (ontology primary namespaces
#' only), or `"prop"` (ontology property namespaces only; may not be exhaustive).
#'
#' @examples
#' # OBO formats
#' obo_id <- c(
#'     #### valid by default ####
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "<http://purl.obolibrary.org/obo/CL_0000066>", # angle-bracketed URI
#'     "DOID:4", # CURIE, standard version
#'     "obo:CL_0000066", # OBO CURIE
#'     #### valid OBO property ####
#'     "so:has_origin", # standard CURIE ~ OBO annotation property
#'     "obo:so#has_origin", # '#' separator ~ OBO annotation property
#'     ### conditionally valid -- only if "ns.lui" in `format` ###
#'     "DOID_0040001", # namespace-separator-LUI (i.e. OBO CURIE w/'obo:' prefix removed)
#'     "so#has_origin", # namespace-lui separator must be '#' for properties
#'     #### invalid ####
#'     "0001816", # bare number without prefix
#'     "obo:DOID:14566", # namespace-lui separator must be '_' ('#' for properties)
#'     " obo:HP_0000001" # must have NO `[:space:]` characters
#' )
#'
#' is_valid_obo(obo_id)
#' is_valid_obo(obo_id, format = c("standard", "ns.lui"))
#'
#' # DOID formats
#' doid <- c(
#'     #### valid ####
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "DOID:4", # CURIE, standard version
#'     "obo:DOID_4", # OBO CURIE, less common
#'     # properties are recognized as valid (unless ns_type = 'ont')
#'     "doid:DO_IEDB_slim",
#'     "obo:doid#DO_IEDB_slim",
#'     # namespace-separator-LUI (i.e. OBO CURIE w/'obo:' prefix removed);
#'     #   only valid if specified in
#'     "DOID_0040001",
#'     #### invalid ####
#'     "0001816", # bare number without prefix
#'     "obo:DOID_21 " # must have NO `[:space:]` characters
#' )
#'
#' is_valid_doid(doid)
#' is_valid_doid(doid, ns_type = "ont")
#'
#' @family ID predicates
#' @family predicates
#' @name obo_ID_predicates
NULL

#' @rdname obo_ID_predicates
#' @export
is_valid_obo <- function(x, format = "standard", ns_type = "any") {
    assert_character(x)
    ns_type <- match.arg(ns_type, c("any", "ont", "prop"))
    prefixes <- switch(
        ns_type,
        any = obo_prefix,
        ont = obo_ont_prefix,
        prop = obo_prop_prefix
    )
    ns_sep <- stringr::str_remove(prefixes, "^.*/")

    if (ns_type == "ont") {
        lui <- "[0-9]+"
    } else {
        lui <- "[[:alnum:]_]+"
    }

    obo_regex <- build_obo_regex(ns_sep, lui, format = format)
    stringr::str_detect(x, obo_regex)
}

#' OBO Property ID Predicate _(DEPRECATED)_
#'
#' **_DEPRECATED_** -- Use `is_valid_obo(x, ns_type = 'prop')` instead.
#' @param x A set of IDs, as a character vector.
#'
#' @export
is_valid_obo_prop <- function(x) {
    rlang::abort("`is_valid_obo_prop()` is deprecated. Use `is_valid_obo(ns_type = 'prop')` instead.",
                 class = "deprecated_function"
    )
}

#' @rdname obo_ID_predicates
#' @export
is_valid_doid <- function(x, format = "standard", ns_type = "any") {
    assert_character(x)
    ns_type <- match.arg(ns_type, c("any", "ont", "prop"))

    ns_sep <- switch(
        ns_type,
        any = c("DOID_", "doid#"),
        ont = "DOID_",
        prop = "doid#"
    )

    if (ns_type == "ont") {
        lui <- "[0-9]+"
    } else {
        lui <- "[[:alnum:]_]+"
    }

    doid_regex <- build_obo_regex(ns_sep, lui, format = format)
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
#' distinguish these use `def = "w3c_safe"` and wrap CURIEs in square brackets
#' (as defined in the standard. Alternatively, use the stricter OBO
#' Foundry-based standards (as stated in that standard, more strict definitions
#' for CURIEs can be defined).
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
#'     "_4dn.biosource:4DNSR73BT2A2", "aceview.worm:aap-1",
#'     #### always fail ####
#'     "4dn.biosource:4DNSR73BT2A2", # starts with a number
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
    if (def == "w3c") pattern <- "^[[:alpha:]_][[:alnum:].-_]*:[[:graph:]]+$"
    if (def == "w3c_safe") pattern <- "^\\[[[:alpha:]_][[:alnum:].-_]+:[[:graph:]]+\\]$"

    stringr::str_detect(x, pattern) & !stringr::str_detect(x, "^_:")
}


#' Test for Valid URIs
#'
#' This is a vectorized predicate to test if character values are valid URLs,
#' i.e. have a scheme and at least one of: hostname or path (according to
#' [RFC3986](https://www.rfc-editor.org/rfc/rfc3986#section-3)).
#'
#' @param x A character vector.
#' @param empty_ok Whether to allow empty hosts and paths, as a boolean
#'    (default: `TRUE`). RFC3986 allows for empty paths & hosts, potentially
#'    making a scheme alone valid and, therefore, the default. However, it is
#'    often desirable to validate that at least one of these is NOT empty, since
#'    a scheme alone is rarely useful in practice.
#'
#' @section Notes:
#' While all URIs are valid CURIEs (see `is_curie(def = "w3c")`), not all CURIEs
#' are valid URIs (e.g. URIs cannot start with `_`).
#'
#' @examples
#' .uri <- c(
#'     # always TRUE
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "https://google.com",
#'     "mailto:fake.name@blah.com",
#'     # TRUE, if empty_ok = FALSE
#'     "file://",
#'     "mailto:",
#'     # never TRUE
#'     "blah",
#'     ""
#' )
#'
#' is_uri(.uri)
#' is_uri(.uri, empty_ok = FALSE)
#'
#' @returns A logical vector indicating which values of `x` are valid URIs.
#'
#' @family ID predicates
#' @family predicates
#' @export
is_uri <- function(x, empty_ok = TRUE) {
    assert_character(x)
    purrr::map_lgl(x, ~ has_uri_reqs(.x, empty_ok))
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
#' @family predicates
#' @export
iff_all_vals <- function(x, values) {
    vals_present <- values %in% x
    only_vals <- all(x %in% values)

    out <- all(vals_present) && all(only_vals)
    if (any(!vals_present)) attr(out, "missing") <- values[!vals_present]
    if (any(!only_vals)) attr(out, "extra") <- x[!only_vals]

    out
}


# Type tests for internal use only ----------------------------------------

is_vctr_or_df <- function(x) {
    is.vector(x) || is.data.frame(x)
}

is_DOrepo <- function(x) {
    class(x)[1] == "pyDOID.repo.DOrepo"
}

is_owl_xml <- function(x) {
    class(x)[1] == "pyDOID.owl.xml"
}

# is_uri() helper
has_uri_reqs <- function(x, empty_ok = TRUE) {
    parsed <- httr::parse_url(x)
    if (empty_ok) {
        path_valid <- !is.null(parsed$path)
        host_valid <- !is.null(parsed$hostname)
    } else {
        # httr uses "/" for empty path when any authority (including empty) is
        # present
        path_valid <- !is.null(parsed$path) && !parsed$path %in% c("", "/")
        host_valid <- !is.null(parsed$hostname) && parsed$hostname != ""
    }

    !is.null(parsed$scheme) && (host_valid || path_valid)
}

# obo_ID_predicates helper
build_obo_regex <- function(ns_sep, lui, format = "standard") {
    format <- match.arg(
        format,
        choices = c("standard", "curie", "obo_curie", "uri", "<uri>", "ns.lui"),
        several.ok = TRUE
    )
    ns_sep <- drop_blank(ns_sep)
    .ns <- paste0(
        "(",
        paste0(stringr::str_remove(ns_sep, ".$"), collapse = "|"),
        ")"
    )
    .ns_sep <- paste0("(", paste0(ns_sep, collapse = "|"), ")")

    formats <- c(
        curie = "{.ns}:{lui}",
        obo_curie = "obo:{.ns_sep}{lui}",
        uri = "http://purl.obolibrary.org/obo/{.ns_sep}{lui}"
    )

    # add <uri> as appropriate
    if ("standard" %in% format || all(c("uri", "<uri>") %in% format)) {
        formats["uri"] <- sandwich_text(formats["uri"], c("<?", ">?"))
    } else if ("<uri>" %in% format) {
        formats["<uri>"] <- sandwich_text(formats["uri"], c("<", ">"))
    }

    # add ns.lui as appropriate
    if ("ns.lui" %in% format && any(c("standard", "obo_curie") %in% format)) {
        formats["obo_curie"] <- "(obo:)?{.ns_sep}{lui}"
    } else if ("ns.lui" %in% format) {
        formats["ns.lui"] <- "{.ns_sep}{lui}"
    }

    if (!"standard" %in% format) {
        formats <- stats::na.omit(formats[format])
    }

    obo_regex <- sandwich_text(
        glue::glue(vctr_to_string(formats)),
        c("^(", ")$")
    )
    obo_regex
}
