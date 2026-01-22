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
#' Append one or more value(s) to a corresponding URL.
#'
#' @section Note:
#' No URL validation is performed.
#'
#' @param x Value(s) to append, as a character vector.
#' @param url One or more URLs or URL names recognized by this package, as a
#'     character vector. If only one value is provided, it will be recycled;
#'     otherwise the length of `url` and `x` must match. See [get_url()] for
#'     recognized base URL names.
#' @param sep One or more separators to use between `url` and `x`, as a
#'     character vector. If only one value is provided (e.g. default = ""), it
#'     will be recycled; otherwise the length of `sep` and `x` must match.
#'     If any `url` ends in the corresponding `sep`, an additional `sep` will
#'     not be added.
#'
#' @examples
#' append_to_url("blah", "http://fake.url.com/")
#'
#' # separator can be specified and will not be duplicated
#' append_to_url("blah", "http://fake.url.com/", sep = "?q=")
#' append_to_url("blah", "http://fake.url.com/", sep = "/")
#'
#' # vectorized w/recycling
#' append_to_url(c("blah", "ugh"), "http://fake.url.com", sep = "/")
#' append_to_url(
#'     c("blah", "ugh"),
#'     c("http://fake.url.com", "https://madeup.url.com/"),
#'     sep = "/"
#' )
#' append_to_url(
#'     c("blah", "ugh"),
#'     c("http://fake.url.com", "https://madeup.url.com/"),
#'     sep = c("/", "?q=")
#' )
#'
#' # missing values in `x` or `url` are preserved
#' append_to_url(
#'     c(NA, "uhhh"),
#'     c("http://fake.url.com", "https://this.is.it.com/"),
#'     sep = "/"
#' )
#' append_to_url(
#'     c(NA, "uhhh"),
#'     c("http://fake.url.com", NA_character_),
#'     c("=", NA)
#' )
#'
#' # `sep` must not be missing for non-missing values of `x` and `url`
#' try(
#'     append_to_url(
#'         c(NA, "uhhh"),
#'         c("http://fake.url.com", "https://this.is.it.com/"),
#'         sep = c("/", NA)
#'     )
#' )
#'
#' # invalid URIs return NA
#' append_to_url("ha", "not_a_url")
#' append_to_url("ha", "unknown.scheme:") # technically a valid URI
#'
#' @export
append_to_url <- function(x, url, sep = "") {
    if (length(url) > 1) {
        if (length(url) != length(x)) {
            rlang::abort('`url` must be the same length as `x` or length == 1')
        }
        url <- purrr::map_chr(
            url,
            ~ tryCatch(get_url(.x), error = function(e) .x)
        )
    } else {
        url <- tryCatch(get_url(url), error = function(e) url)
    }

    if (length(sep) > 1 && length(sep) != length(x)) {
        rlang::abort('`sep` must be the same length as `x` or length == 1')
    }

    ignore_sep <- purrr::map2_lgl(
        url,
        sep,
        ~ stringr::str_detect(.x, paste0(stringr::str_escape(.y), "$"))
    ) %>%
        tidyr::replace_na(TRUE)

    if (all(ignore_sep)) {
        new_url <- paste0(url, x)
    } else {
        new_url <- purrr::pmap_chr(
            list(url, x, sep, ignore_sep),
            function(.u, .x, .s, .i) {
                if (.i) {
                    paste0(.u, .x)
                } else {
                    paste(.u, .x, sep = .s)
                }
            }
        )
    }

    # ensure output are valid URLs; otherwise return NA
    not_url <- !is_uri(new_url, empty_ok = FALSE)
    new_url[is.na(x) | is.na(url) | not_url] <- NA
    new_url
}
