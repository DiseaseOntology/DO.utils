#' Collapse invariant vectors
#'
#' Returns the unique value from a vector of any length if and only if only 1
#' unique value exists, otherwise returns the original vector
#'
#' @param x vector
#'
#' @export
unique_if_invariant <- function(x) {
    if (dplyr::n_distinct(x) == 1) {
        return(unique(x))
    }
    x
}
