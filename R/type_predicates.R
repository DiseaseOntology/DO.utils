#' Invariant test
#'
#' Test if a vector is invariant, i.e. has only one value
#'
#' @inheritParams dplyr::n_distinct
#'
#' @export
is_invariant <- function(..., na.rm = FALSE) {
    dplyr::n_distinct(..., na.rm = na.rm) == 1
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


# Type tests for internal use only
is_vctr_or_df <- function(x) {
    is.vector(x) || is.data.frame(x)
}
