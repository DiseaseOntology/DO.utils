#' Append Empty Columns
#'
#' Appends empty columns (value = `NA`) to a data.frame.
#'
#' @param df A data.frame.
#' @param col The name(s) of one or more columns desired in the final
#'     data.frame, as a character vector. Any names not currently in the
#'     data.frame will be added as empty columns; those present will remain
#'     unchanged.
#' @param order Whether to reorder the data.frame to match the order of `col`,
#'     as a boolean (default: `FALSE`). If `FALSE` empty columns are added to the
#'     right. When `order = TRUE`, `col` is used to specify column order. Thus,
#'     in addition to the names of the empty columns to append, `col` must
#'     include all column names in `df`. `append_empty_col()` will not
#'     subset/select columns.
#'
#' @export
append_empty_col <- function(df, col, order = FALSE) {
    stopifnot(is_boolean(order))
    stopifnot(is.character(col))

    new_col <- col[!col %in% names(df)]
    empty_df <- data.frame(
        matrix(ncol = length(new_col), nrow = nrow(df)),
        stringsAsFactors = FALSE
    )
    colnames(empty_df) <- new_col

    out_df <- dplyr::bind_cols(df, empty_df)

    if (order) {
        col_nm <- names(out_df)
        if (!all(col_nm %in% col)) {
            rlang::abort(
                message = c(
                    "All column names in output must be in `col`. Names missing:",
                    col_nm[!col_nm %in% col]
                )
            )
        }

        out_df <- dplyr::select(out_df, dplyr::one_of(col))
    }
    out_df
}


#' Append to URL
#'
#' Append a value to a URL.
#'
#' @section Note:
#' No URL validation is performed.
#'
#' @param x value to append
#' @param url a URL or the internal name of a URL used in this package (see
#' [get_url] for possible names)
#' @param preserve_NA Whether to preserve `NA` in output, as a boolean. `FALSE`
#' will result in `NA` being appended to the end of `url` (almost certainly not
#' desired).
#'
#' @export
append_to_url <- function(x, url, preserve_NA = TRUE) {

    url <- tryCatch(get_url(url), error = function(e) url)

    # add '/' if no terminal '/' in URL
    if (stringr::str_detect(url, "/$")) {
        new_url <- paste0(url, x)
    } else {
        new_url <- paste0(url, "/", x)
    }

    if (preserve_NA) {
        new_url[is.na(x)] <- NA
    }

    new_url
}
