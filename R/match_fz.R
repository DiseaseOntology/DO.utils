#' Fuzzy (Approximate) String Matching
#'
#' Wraps [stringdist::amatch()] to perform "fuzzy" (approximate) string
#' matching while providing more informative output. Instead of an integer
#' vector of best match positions, this function returns a tibble with the
#' input, its corresponding best match, and the approximate string distance.
#'
#' @section NOTES:
#' Fuzzy string matching is _SLOW_. Expect this function to take >1 min for
#' comparisons of more than 500 values for all methods.
#'
#' For comparison of citation titles specifically, the "lcs" method is faster
#' than "osa" and seems to work better. Based on light experimentation, a good
#' setting for `maxDist` value for citation titles is between 80-115.
#'
#' @inheritParams stringdist::amatch
#' @param ... arguments passed on to [stringdist::amatch()]
#'
#' @return
#' A tibble with 3 columns:
#' * `x`
#' * `table_match`: the closest match of `x`
#' * `dist`: the distance between x and its closest match (given the method
#' selected
#'
#' @export
match_fz <- function(x, table, method = "lcs", maxDist = 115, ...) {

    table_match_idx <- stringdist::amatch(
        x,
        table,
        method = method,
        maxDist = maxDist,
        ...
    )

    # create df of set 1 & 2 citations
    match_df <- tibble::tibble(
        x = x,
        table_match = table[table_match_idx]
    )

    match_df <- dplyr::mutate(
        match_df,
        dist = purrr::map2_dbl(
            # $ syntax to ensure match within df
            .x = match_df$x,
            .y = match_df$table_match,
            ~ stringdist::stringdist(.x, .y, method = method)
        )
    )

    match_df
}
