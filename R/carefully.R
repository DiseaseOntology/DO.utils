#' Matches Carefully
#'
#' Wrapper for [base::match()] that will NOT match `NA` values. Uses
#' [dplyr::if_else()] to skip `NA` values.
#'
#' @inheritParams base::match
#'
#' @export
#'
#' @family carefully
match_carefully <- function(x, table, nomatch = NA_integer_,
                            incomparables = NULL) {
    dplyr::if_else(
        is.na(x),
        NA_integer_,
        match(x, table, nomatch, incomparables)
    )
}
