#' Drop Blanks
#'
#' Drops "" values from a vector
#'
#' @param x character vector
#'
#' @export
drop_blank <- function(x) {
    x[!is_blank(x)]
}
