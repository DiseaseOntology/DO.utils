#' Collapse Column(s)
#'
#' Collapses values in the column(s) specified, concatenating unique values in
#' those columns for each record (where a record is defined as a unique
#' row including all columns _NOT_ specified in `.cols`).
#'
#' @param df a data.frame
#' @param .cols the name of the columns in the data.frame to collapse
#' @param ... arguments passed on to [unique_if_invariant]
#' @inheritParams vctr_to_string
#'
#' @examples
#' cc_df <- tibble::tibble(
#'      w = c(1, 2, 3, 3, 4, 4, 4, 4),
#'      x = c("a", "a", "b", "b", "c", "c", "e", "e"),
#'      y = c("Z", "Y", "X", "X", "W", "V", "U", "U"),
#'      z = c(1.1, 1.3, 1.4, 1.4, 2.0, 1.45, 1.46)
#' )
#' cc_df
#'
#' # completely duplicated rows (3-4) are collapsed with any .cols specified
#' collapse_col(cc_df, x)
#'
#' collapse_col(cc_df, w)
#' collapse_col(cc_df, y)
#' collapse_col(cc_df, c(w, y))
#'
#' # numeric vectors with differences below the default/specified tolerance
#' # are collapsed to the mean, otherwise values are concatenated and .col is
#' # convertedd to character
#' collapse_col(cc_df, z) # difference in row 7-8 above default tol --> character
#' collapse_col(cc_df, z, tol = 0.2) # difference below tol --> mean
#'
#' @export
collapse_col <- function(df, .cols, ..., delim = "; ") {
    col_order <- names(df)

    df %>%
        dplyr::group_by(dplyr::across(-{{ .cols }})) %>%
        dplyr::summarize(
            #note: summarize sorts = row order is not preserved
            across(
                .cols = {{ .cols }},
                .fns = ~ vctr_to_string(
                    unique_if_invariant(.x, ...),
                    delim = delim
                )
            )
        ) %>%
        dplyr::ungroup() %>%
        # restore column order
        dplyr::select(dplyr::one_of(col_order))
}
