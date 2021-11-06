#' Return Unique Value for Invariant Vectors
#'
#' Returns the unique value from a vector of any length, if and only if, only 1
#' unique value exists (_i.e._ the vector is invariant), otherwise returns the
#' original vector.
#'
#' @inheritParams is_invariant
#'
#' @seealso For _unconditional_ vector-to-string conversion methods, see the
#'     [vctr_to_string()] family of functions.
#'
#' @export
unique_if_invariant <- function(x, na.rm = FALSE, ...) {
    UseMethod("unique_if_invariant")
}

#' @export
#' @rdname unique_if_invariant
unique_if_invariant.character <- function(x, na.rm = FALSE, ...) {
    if (is_invariant(x, na.rm = na.rm, ...)) {
        return(unique(x))
    }
    x
}

#' @export
#' @rdname unique_if_invariant
unique_if_invariant.numeric <- function(x, na.rm = FALSE,
                                        tol = sqrt(.Machine$double.eps), ...) {
    if (is_invariant(x, na.rm = na.rm, tol = tol, ...)) {
        return(mean(x, na.rm = na.rm))
    }
    x
}


#' Convert Vectors to Strings
#'
#' Concatenate values in a vector into a single string.
#'     * `vctr_to_string` performs simple concatenation.
#'     * `unique_to_string` reduces the vector to unique values prior to
#'         concatenation.
#'
#' @param x A vector.
#' @param delim A delimiter to place between vector elements (default: "|").
#' @param na.rm A logical scalar indicating whether `NA` values should be
#'     removed (default: `FALSE`).
#'
#' @seealso [unique_if_invariant()] for an alternative, _conditional_ `unique`
#'     vector-to-string conversion method
#'
#' @export
vctr_to_string <- function(x, delim = "|", na.rm = FALSE) {
    assert_scalar_logical(na.rm)

    if (na.rm) {
        x <- stats::na.omit(x)
    }

    paste0(x, collapse = delim)
}

#' @rdname vctr_to_string
#' @export
unique_to_string <- function(x, delim = "|", na.rm = FALSE) {
    vctr_to_string(unique(x), delim = delim, na.rm = na.rm)
}
