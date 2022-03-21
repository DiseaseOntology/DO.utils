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

#' DO-specific predicate
#'
#' This predicate is designed to identify and validate common formats for DOIDs
#' derived from the Human Disease Ontology (DO). Note that bare numbers are NOT
#' recognized as valid DOIDs, a prefix is _always_ required.
#'
#' @param x A set of DOIDs, as a character vector.
#'
#' @examples
#' # formats considered valid
#' is_valid_doid("http://purl.obolibrary.org/obo/DOID_0001816") # URI
#' is_valid_doid("DOID:4") # CURIE, standard version
#' is_valid_doid("obo:DOID_14566") # obo CURIE, less common
#' is_valid_doid("DOID_0040001") # basename (prefix removed)
#'
#' # invalid formats
#' is_valid_doid(c("0001816", "4")) # bare numbers
#' is_valid_doid("obo:DOID:14566") # some specificity for separators is enforced
#'
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
