#' Create a String from Inputs
#'
#' Creates a single string from one or more inputs with each value separated by
#' `delim`.
#'
#' @param ... One or more R objects (vector, list, data.frame, etc).
#' @inheritParams vctr_to_string
#' @param unique Whether to include only unique values (across all inputs), as
#'     a logical.
#'
#' @examples
#' # collapses individual vectors
#' collapse_to_string(1:10)
#'
#' # input order is preserved
#' collapse_to_string(1:2, letters[1:2])
#' collapse_to_string(data.frame(x = 1:2, y = letters[1:2]))
#'
#' # factor levels are captured (instead of numeric placeholders)
#' collapse_to_string(factor(letters[1:2]), "c")
#'
#' # unique applies across all inputs, order is determined by first appearance
#' collapse_to_string(c(3, 1, 2), 1:4, unique = FALSE)
#' collapse_to_string(c(3, 1, 2), 1:4, unique = TRUE)
#'
#' @export
collapse_to_string <- function(..., delim = "|", na.rm = FALSE, unique = FALSE) {
    assert_scalar_logical(na.rm)

    x <- to_character(list(...))
    if (unique) {
        string <- unique_to_string(x, delim = delim, na.rm = na.rm)
    } else {
        string <- vctr_to_string(x, delim = delim, na.rm = na.rm)
    }

    string
}


#' Collapse Column(s)
#'
#' Collapses values in the column(s) specified, concatenating unique values in
#' those columns for each record (where a record is defined as a unique
#' row including all columns _NOT_ specified in `.cols`).
#'
#' @param df a data.frame
#' @param .cols the name of the column in the data.frame to collapse
#' @param method A string identifying a function to use; one of "unique",
#'     "first", or "last".
#' @inheritParams vctr_to_string
#'
#' @return
#' A data.frame with the specified columns collapsed. Also **NOTE** the
#' following:
#'
#' 1. Collapsed columns **will** be converted to `character`.
#' 2. Rows will be reordered by the unique combination of columns _not_
#' collapsed (due to [dplyr::group_by()] use).
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
#' # negative & tidy selection works; all equivalent to collapse_col(cc_df, c(x, z))
#' collapse_col(cc_df, -y)
#' collapse_col(cc_df, dplyr::matches("x|z"))
#' collapse_col(cc_df, -dplyr::all_of("y"))
#'
#' @seealso [collapse_col_flex()] for a more flexible approach and
#' [lengthen_col()] for the pseudo-reverse operation that lengthens/expands
#' one or more specified columns.
#'
#' @export
collapse_col <- function(df, .cols, delim = "|", method = "unique",
                         na.rm = FALSE) {
    valid_methods <- c("unique", "first", "last")
    method <- match.arg(method, choices = valid_methods)
    df %>%
        dplyr::group_by(dplyr::across(-{{ .cols }})) %>%
        dplyr::summarize(
            dplyr::across(
                .cols = dplyr::everything(),
                .fns = ~ collapse_method(
                    .x,
                    method = method,
                    delim = delim,
                    na.rm = na.rm
                )
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(names(df)))
}


#' Collapse Column(s) Flexibly
#'
#' Collapses values in the column(s) specified using a defined method for each
#' record, where a record is defined as a unique observation comprised of all
#' columns _NOT_ specified in `collapse`).
#'
#' @param df A data.frame
#' @param ... Column-method pairs specifying the `method` to use for each column
#'     to be collapsed; column names can be bare variables or strings, methods
#'     must be strings, e.g. column_name = method.
#' @param method A string identifying a function to use; one of "unique",
#'     "first", or "last"
#' @inheritParams vctr_to_string
#'
#' @return
#' A data.frame with the specified columns collapsed. Also **NOTE** the
#' following:
#'
#' 1. _For all methods_, rows will be reordered by the unique combination of
#' columns _not_ collapsed (due to [dplyr::group_by()] use).
#' 2. _For "unique" method, collapsed columns **will** be converted to
#' `character`.
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
#' # individual columns
#' collapse_col_flex(cc_df, z, method = "unique")
#' collapse_col_flex(cc_df, z, method = "first")
#' collapse_col_flex(cc_df, z, method = "last")
#'
#' # multiple columns can be collapsed using the same method
#' collapse_col_flex(cc_df, x, z, method = "unique")
#' collapse_col_flex(cc_df, x, z, method = "first")
#' collapse_col_flex(cc_df, x, z, method = "last")
#'
#' # ...or using different methods
#' collapse_col_flex(cc_df, x = "unique", z = "unique")
#' collapse_col_flex(cc_df, x = "first", z = "unique")
#' collapse_col_flex(cc_df, x = "first", z = "last")
#'
#' @seealso [collapse_col()] for a simpler approach and [lengthen_col()] for the
#' pseudo-reverse operation that lengthens/expands one or more specified columns.
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
                .cols = dplyr::everything(),
                .fns = ~ collapse_method(
                    .x,
                    c_method[[dplyr::cur_column()]],
                    delim = delim
                )
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(names(df)))
}
