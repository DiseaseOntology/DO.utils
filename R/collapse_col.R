#' Collapse Column(s) Flexibly
#'
#' Collapses values in the column(s) specified using a defined method for each
#' record, where a record is defined as a unique observation comprised of all
#' columns _NOT_ specified in `collapse`).
#'
#' @param df A data.frame
#' @param collapse A list of columns to collapse using `method` or a named
#'     list specifying the method for each column, e.g. column_name = method.
#' @param method A string identifying a function to use; one of "unique",
#'     "first", or "last"
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
collapse_col_flex <- function(df, ..., method = "unique",
                         delim = "|") {

    # validate method argument
    valid_methods <- c("unique", "first", "last")
    method <- match.arg(method, choices = valid_methods)

    dots_as_strings <- rlang::enexprs(...) %>%
        purrr::map(rlang::as_string)

    if (any(names(dots_as_strings) != "")) {
        assertthat::assert_that(all(names(dots_as_strings) %in% names(df)))
        assertthat::assert_that(
            all(purrr::map_int(dots_as_strings, length) == 1),
            all(dots_as_strings %in% valid_methods)
        )

        collapse_vars <- names(dots_as_strings)
        c_method <- dots_as_strings
    } else {
        assertthat::assert_that(all(dots_as_strings %in% names(df)))
        collapse_vars <- unlist(dots_as_strings)
        c_method <- purrr::set_names(
            rep(method, length(collapse_vars)),
            nm = collapse_vars
        )
    }

    df %>%
        dplyr::group_by(dplyr::across(-{{ collapse_vars }})) %>%
        dplyr::summarize(
            dplyr::across(
                .cols = {{ collapse_vars }},
                .fns = ~ collapse_method(
                    .x,
                    c_method[[dplyr::cur_column()]],
                    delim = delim
                )
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::one_of(names(df)))
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
