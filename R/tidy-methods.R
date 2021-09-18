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
    tidied <- x %>%
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")
}
