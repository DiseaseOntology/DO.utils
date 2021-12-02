#' Collapse Column(s)
#'
#' Collapses values in the column(s) specified, concatenating unique values in
#' those columns for each record (where a record is defined as a unique
#' row including all columns _NOT_ specified in `.cols`).
#'
#' @param df a data.frame
#' @param .cols the name of the column in the data.frame to collapse
#' @inheritParams vctr_to_string
#'
#' @examples
#' cc_df <- tibble::tibble(
#'      x = c(1, 2, 3, 3, 4, 4, 4),
#'      y = c("a", "a", "b", "b", "c", "c", "e"),
#'      z = c("Z", "Y", "X", "X", "W", "V", "U")
#' )
#' cc_df
#'
#' # completely duplicated rows (3-4) are collapsed with any .cols specified
#' collapse_col(cc_df, y)
#'
#' collapse_col(cc_df, x)
#' collapse_col(cc_df, z)
#' collapse_col(cc_df, c(x, z))
#'
#' @export
collapse_col <- function(df, .cols, delim = "|") {
    df %>%
        dplyr::group_by(dplyr::across(-{{ .cols }})) %>%
        dplyr::summarize(
            dplyr::across(
                .cols = {{ .cols }},
                .fns = ~ vctr_to_string(
                    unique(.x),
                    delim = delim
                )
            )
        ) %>%
        dplyr::ungroup()
}
