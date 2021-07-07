#' Collapse invariant vectors
#'
#' Returns the unique value from a vector of any length if and only if only 1
#' unique value exists, otherwise returns the original vector
#'
#' @param x vector
#'
#' @export
#' @family vector-to-scalar functions
unique_if_invariant <- function(x) {
    if (dplyr::n_distinct(x) == 1) {
        return(unique(x))
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
    string <- paste0(x, collapse = delim)
}
