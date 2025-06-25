#' Count Delimited Columns
#'
#' Counts columns when values within them are delimited.
#'
#' @param data A data.frame.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables to first
#'  lengthen, then count by.
#' @inheritParams lengthen_col
#' @inheritParams dplyr::count
#'
#' @export
count_delim <- function(data, ..., delim = "|", sort = FALSE, name = NULL,
                        trim = TRUE, convert = FALSE) {
    # Convert data-mask to tidy selection for lengthen_col()
    # As described in rlang documentation: https://rlang.r-lib.org/reference/topic-data-mask-programming.html#transmute-as-a-data-mask-to-selection-bridge
    inputs <- dplyr::transmute(data, ...)
    col_nm <- names(inputs)
    col_injected <- !col_nm %in% names(data)
    if (any(col_injected)) {
        rlang::warn(
            "Columns have been injected into `data` with `...`.\nDid you accidentally quote an column name? (e.g. \"x\", instead of .data$x)"
        )
    }
    data <- dplyr::mutate(data, !!!inputs)

    # Use lengthen_col on the selected columns
    .df <- lengthen_col(
        data,
        cols = dplyr::all_of(col_nm),
        delim = delim,
        trim = trim,
        convert = convert
    )

    # Use dplyr::count with data-masking
    out <- dplyr::count(.df, ..., sort = sort, name = name)

    out
}
