#' Drop Blanks
#'
#' Drops blank values from a vector or list. See [is_blank()] for what
#' constitutes a blank value. `drop_blank()` is a generic function.
#'
#' @param x A character vector or list.
#'
#' @returns An object of the same class and length as `x`.
#' @examples
#' drop_blank(c("", "A")
#' drop_blank(
#'     list(
#'         c("", "A"),
#'         c("A", "B"),
#'         c("C", "D", "", "E", "")
#'     )
#' )
#'
#' @export
drop_blank <- function(x) {
    UseMethod("drop_blank")
}

#' @export
drop_blank.character <- function(x) {
    x[!is_blank(x)]
}

#' @export
drop_blank.list <- function(x) {
    purrr::map(x, ~ .x[!is_blank(.x)])
}
