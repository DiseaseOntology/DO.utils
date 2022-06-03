#' Unnest data frame list columns
#'
#' Like [tidyr::unnest()] but always produces a cartesian product and does not
#' requires that list columns be "parallel entries ... of compatible sizes".
#'
#' @param data A data.frame.
#' @param cols <[`tidy-select`][tidyr_tidy_select]> Columns to unnest.
#' @inheritDotParams tidyr::unchop
#' @inheritDotParams tidyr::unpack
#'
#' @examples
#' df <- tibble::tibble(
#'     n = 1:2,
#'     interjection = list(c("oh", "wow"), "zap"),
#'     letter = list(c("a", "b", "c"), "d"),
#'     keep_list = list("please", c("don't", "unnest"))
#' )
#' df
#'
#' # Uses tidyselect semantics, like tidyr::unnest()
#' unnest_cross(df, cols = tidyselect::starts_with("inter"))
#'
#' # Works when list column sizes differ, unlike tidyr::unnest()
#' unnest_cross(df, cols = c(interjection, letter))
#' tryCatch(
#'     tidyr::unnest(df, cols = c(interjection, letter)),
#'     error = function(e) message(e)
#' )
#'
#' # Always produces a cartesian product, unlike tidyr::unnest()
#' df2 <- tibble::tibble(
#'     n = list(1:2, 3L),
#'     letter = list(c("a", "b"), "c"),
#' )
#' df2
#'
#' tidyr::unnest(df2, cols = tidyselect::everything())
#' unnest_cross(df2, cols = tidyselect::everything())
#'
#' @export
unnest_cross <- function(data, cols, ...) {
    .df_out <- data
    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    purrr::walk(
        .cols,
        function(col) {
            .df_out <<- tidyr::unnest(.df_out, {{ col }}, ...)
        }
    )
    .df_out
}
