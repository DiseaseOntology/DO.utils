#' Test if Vector is Invariant
#'
#' Test if a vector is invariant (_i.e._ all values are equal, within a given
#' tolerance for numeric vectors).
#'
#' @param x vector to be tested
#' @param na.rm logical indicating whether to exclude NA values
#' @param tol double, tolerance to use (for numeric vectors)
#' @param ... unused; for extensibility
#'
#' @family type_predicates
#' @export
is_invariant <- function(x, na.rm = FALSE, ...) {
    UseMethod("is_invariant")
}

#' @export
#' @rdname is_invariant
is_invariant.character <- function(x, na.rm = FALSE, ...) {
    dplyr::n_distinct(x, na.rm = na.rm) == 1
}

#' @export
#' @rdname is_invariant
is_invariant.numeric <- function(x, na.rm = FALSE,
                                 tol = sqrt(.Machine$double.eps), ...) {
    diff(range(x, na.rm = na.rm)) < tol
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
#' @family type_predicates
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
#' @family type_predicates
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
#' @name lgl_predicates
NULL

#' @export
#' @rdname lgl_predicates
is_boolean <- function(x) {
    is.logical(x) & length(x) == 1
}


#' ID predicates
#'
#' These predicates are designed to identify and validate common ID formats
#' defined within OBO Foundry ontologies.
#'
#' * `is_valid_obo()` identifies NA's and blanks
#'
#' * `is_valid_doid()` identifies "" or whitespace of any length
#'
#' @section Notes:
#' These predicates _do not_ attempt to confirm any ID actually exists in an
#' OBO Foundry ontology, but only test if the IDs are syntactically formatted
#' correctly (see [OBO Foundry ID Policy](https://obofoundry.org/id-policy) and
#' [OBO File Specification](https://owlcollab.github.io/oboformat/doc/obo-syntax.html)).
#'
#' Not all OBO formats are valid DOID formats and vice versa.
#'
#' @param x A set of IDs, as a character vector.
#'
#' @examples
#' # OBO formats
#' obo_id <- c(
#'     # valid
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "<http://purl.obolibrary.org/obo/CL_0000066>", # bracketed_URI
#'     "obo:DOID_4", # CURIE, standard version
#'     "http://purl.obolibrary.org/obo/so#has_origin", # namespace-lui '#' separator ~ OBO annotation properties
#'     # invalid
#'     "0001816", # bare number without prefix
#'     "obo:DOID:14566" # namespace-lui separator must be '_' or '#'
#'     " obo:HP_0000001" # must have NO `[:space:]` characters
#' )
#'
#' is_valid_obo(obo_id)
#'
#' # DOID formats
#' doid <- c(
#'     # valid
#'     "http://purl.obolibrary.org/obo/DOID_0001816", # URI
#'     "DOID:4", # CURIE, standard version
#'     "obo:DOID_4", # OBO CURIE, less common
#'     "DOID_0040001", # basename (OBO prefix removed)
#'     # invalid
#'     "0001816", # bare number without prefix
#'     "doid#DO_IEDB_slim", # namespace-lui separator must be '_'
#'     "obo:doid#DO_IEDB_slim"
#'     "obo:DOID_21 " # must have NO `[:space:]` characters
#' )
#'
#' is_valid_doid(doid)
#'
#' @family type_predicates
#' @name ID_predicates
NULL

#' @rdname ID_predicates
#' @export
is_valid_obo <- function(x) {
    assert_character(x)
    obo_regex <- "^<?http://purl.obolibrary.org/obo/[A-Za-z_]+[_#][[:alnum:]_]+>?$|^obo:[A-Za-z_]+[_#][[:alnum:]_]+$"
    stringr::str_detect(x, obo_regex)
}

#' @rdname ID_predicates
#' @export
is_valid_doid <- function(x) {
    assert_character(x)
    doid_regex <- "^(http://purl.obolibrary.org/obo/|obo:)?DOID_[0-9]{1,7}$|^DOID:[0-9]{1,7}$"
    stringr::str_detect(x, doid_regex)
}


# Type tests for internal use only
is_vctr_or_df <- function(x) {
    is.vector(x) || is.data.frame(x)
}

is_owl_xml <- function(x) {
    class(x)[1] == "pyDOID.owl.xml"
}
