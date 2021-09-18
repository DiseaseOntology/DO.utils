#' Tidy an `esummary_list` object
#'
#' Tidy converts an `esummary_list` into a [tibble][tibble::tibble].
#'
#' @param x An `esummary_list` object created by [rentrez::entrez_summary]
#' (for single inputs `always_return_list` must be TRUE) and it's derivatives
#' ([pubmed_summary]).
#' @param ... Additional arguments. Not used.
#'
#' @return A tibble, where `uid` is the esummary input identifier
#' @export
tidy.esummary_list <- function(x, ...) {
    x %>%
        tibble::enframe(name = "tmp1", value = "tmp2") %>%
        tidyr::unnest_wider(col = "tmp2") %>%
        dplyr::select(-tmp1)
}
