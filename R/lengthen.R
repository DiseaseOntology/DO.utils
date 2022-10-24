#' Lengthen Column(s)
#'
#' Lengthens values in the column(s) specified, de-concatenating values in
#' those columns, resulting in duplicated rows that differ only by values in
#' `cols`. NOTE: `lengthen_col()` is not the exact reverse of [collapse_col()];
#' see examples.
#'
#' @param data A data.frame.
#' @param cols The name of the column(s) in the data.frame to lengthen.
#' @param delim A delimiter to split elements within specified columns by
#' (default: "|").
#' @param trim Whether to trim start/end whitespace, as a boolean
#' (default: `TRUE`).
#' @param convert Whether to run [type.convert()](utils::type.convert) with
#' `as.is = TRUE` on new columns. This is useful if the de-concatenated columns
#' are integer, numeric or logical. NOTE: "NA" strings will _always_ be
#' converted to `NA`s.
#'
#' @return
#' A data.frame with the specified columns lengthened.
#'
#' @examples
#' z_unique <- tibble::tibble(
#'   x = c(1, 2, 3, 4, 4),
#'   y = c("a", "a", "b", "c", "e"),
#'   z = c("Z", "Y", "X", "W|V", "U")
#' )
#'
#' lengthen_col(z_unique, z)
#'
#' # Data will likely differ after round trip through `collapse_col()` and
#' # `lengthen_col()` because:
#' #    1. Duplicate rows (cc_df, row 4) are lost
#' #    2. New crosses are created (cc_df2, row 6-8)
#' #    3. Output is sorted by `cols` due to use of `dplyr::group_by()`
#' #       internally.
#' cc_df <- tibble::tibble(
#'   x = c(1, 2, 3, 3, 4, 4, 4),
#'   y = c("a", "a", "b", "b", "c", "c", "e"),
#'   z = c("Z", "Y", "X", "X", "W", "V", "U")
#' )
#'
#' cc_df2 <- lengthen_col(
#'   collapse_col(cc_df, c(y, z)),
#'   c(y, z)
#' )
#'
#' if (rlang::is_installed("waldo")) {
#'   waldo::compare(cc_df, cc_df2)
#' } else {
#'   cc_df
#'   cc_df2
#' }
#'
#' @seealso collapse_col
#' @export
lengthen_col <- function(data, cols, delim = "|", trim = TRUE, convert = FALSE) {
    assert_scalar_logical(trim)
    assert_scalar_logical(convert)

    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    df_sep <- dplyr::mutate(
        data,
        dplyr::across(
            {{ .cols }},
            .fns = ~ stringr::str_split(.x, stringr::coll(delim)),
        )
    )

    df_long <- unnest_cross(df_sep, .cols, keep_empty = TRUE)
    if (trim) {
        df_long <- dplyr::mutate(
            df_long,
            dplyr::across(.cols, stringr::str_trim)
        )
    }
    if (convert) {
        df_long <- dplyr::mutate(
            df_long,
            dplyr::across(.cols, utils::type.convert, as.is = TRUE)
        )
    } else {
        df_long <- dplyr::mutate(
            df_long,
            dplyr::across(.cols, dplyr::na_if, y = "NA")
        )
    }

    df_long
}
