#' Collapse Column(s) Flexibly
#'
#' Collapses values in the column(s) specified using a defined method for each
#' record, where a record is defined as a unique observation comprised of all
#' columns _NOT_ specified in `collapse`).
#'
#' @param df A data.frame
#' @param collapse A list of columns to collapse using `method` or a named
#'     list specifying the method for each column, e.g. column_name = method.
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
#' # completely duplicated rows (3-4) are collapsed with any column(s) specified
#' collapse_col_flex(cc_df, y)
#'
#' collapse_col_flex(cc_df, x)
#' collapse_col_flex(cc_df, z)
#' collapse_col_flex(cc_df, c(x, z))
#'
#' @export
collapse_col_flex <- function(df, collapse = list(), method = "unique",
                         delim = "|") {

    # validate arguments
    valid_methods <- c("unique", "first", "last")
    assertthat::assert_that(rlang::is_list(collapse))
    method <- match.arg(method, choices = valid_methods)

    if (!is.null(names(collapse))) {
        # validate collapse argument further if named list
        assertthat::assert_that(all(names(collapse) %in% names(df)))
        assertthat::assert_that(
            all(purrr::map_int(collapse, length) == 1),
            all(collapse %in% valid_methods)
        )

        collapse_vars <- names(collapse)
        method <- collapse
    } else {
        assertthat::assert_that(all(collapse %in% names(df)))
        collapse_vars <- collapse
    }

    df %>%
        dplyr::group_by(dplyr::across(-{{ collapse_vars }})) %>%
        dplyr::summarize(
            dplyr::across(
                .cols = {{ collapse_vars }},
                .fns = ~ collapse_method(
                    .x,
                    method[[dplyr::cur_column()]],
                    delim = delim
                )
            )
        ) %>%
        dplyr::ungroup()
}

# collapse_col_flex() helper (internal)
collapse_method <- function(.col, method = "unique", delim = "|") {
    method_fxn <- list(
        unique = unique,
        first = dplyr::first,
        last = dplyr::last
    )

    vctr_to_string(method_fxn[[method]](.col), delim = delim)
}


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
