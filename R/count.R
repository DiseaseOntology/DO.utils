#' Count Delimited Columns
#'
#' Counts columns when values within them are delimited.
#'
#' @param data A data.frame.
#' @inheritParams lengthen_col
#' @inheritParams dplyr::count
#'
#' @export
count_delim <- function(data, ..., delim = "|", sort = FALSE, name = NULL,
                        trim = TRUE, convert = FALSE) {
    .col <- vapply(rlang::exprs(...), rlang::as_string, character(1))
    .df <- lengthen_col(data, .col)
    out <- dplyr::count(.df, ..., sort = sort, name = name)

    out
}
