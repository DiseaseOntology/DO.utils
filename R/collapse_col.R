#' Collapse Column
#'
#' @param df a data.frame
#' @param .col the name of the column in the data.frame to collapse
#' @inheritParams vctr_to_string
#'
#' @export
collapse_col <- function(df, .col, delim = "; ") {
    df %>%
        dplyr::group_by(dplyr::across(-{{ .col }})) %>%
        dplyr::summarize(
            {{ .col }} := vctr_to_string(
                unique({{ .col }}),
                delim = delim
            )
        ) %>%
        dplyr::ungroup()
}
