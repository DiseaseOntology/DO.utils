#' Identify Citations of Closest Match
#'
#' From two sets of citations, identifies the closes match for set 1 in set 2
#' below a threshold (`maxDist`) using fuzzy string matching.
#'
#' @section NOTES:
#' Fuzzy string matching is _SLOW_. Expect this function to take >1 min for
#' comparisons of more than 500 citations for all methods.
#'
#' "lcs" method is faster than "osa" and seems to work better.
#'
#'
#' @param x citations to find matches for
#' @param ref citations to find matches from
#' @inheritParams stringdist::amatch
#' @param ... arguments passed on to [stringdist::amatch()]
#'
#' @return A tibble with x, the reference most closely matching x, and the
#' distance between x and the reference match (based on the selected method);
#' columns = "x", "ref_match", "dist".
#'
#' @export
match_citations_fz <- function(x, ref, method = "lcs", maxDist = 115, ...) {

    ref_match_idx <- stringdist::amatch(
        x,
        ref,
        method = method,
        maxDist = maxDist,
        ...
    )

    # create df of set 1 & 2 citations
    match_df <- tibble::tibble(
        x = x,
        ref_match = ref[ref_match_idx]
    )

    match_df <- dplyr::mutate(
        match_df,
        dist = purrr::map2_dbl(
            # $ syntax to ensure match within df
            .x = match_df$x,
            .y = match_df$ref_match,
            ~ stringdist::stringdist(.x, .y, method = method)
        )
    )

    match_df
}

