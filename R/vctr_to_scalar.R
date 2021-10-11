#' Collapse invariant vectors
#'
#' Returns the unique value from a vector of any length if and only if only 1
#' unique value exists, otherwise returns the original vector
#'
#' @inheritParams is_invariant
#'
#' @export
#' @family vector-to-scalar functions
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


#' Collapse vector to string
#'
#' Concatenates values in a vector into a single string.
#'
#' @param x vector
#' @param delim delimiter to place between vector elements
#'
#' @export
#' @family vector-to-scalar functions
vctr_to_string <- function(x, delim = "; ") {
    paste0(x, collapse = delim)
}
