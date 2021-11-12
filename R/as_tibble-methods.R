#' Convert an `esummary_list` to a Tibble
#'
#' Converts an `esummary_list` into a [tibble][tibble::tibble].
#'
#' @param x An `esummary_list` object created by [rentrez::entrez_summary]
#' (for single inputs `always_return_list` must be TRUE) and it's derivatives
#' ([pubmed_summary]).
#' @param ... Additional arguments. Not used.
#'
#' @return A tibble, where `uid` is the esummary input identifier
#' @export
as_tibble.esummary_list <- function(x, ...) {
    x %>%
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")
}

#' @export
as_tibble.esummary_list_nested <- function(x, ...) {
    purrr::map(x, as_tibble.esummary_list) %>%
        dplyr::bind_rows(.id = "cites")
}

#' @export
as_tibble.scopus_search <- function(x, ...) {
    tbl_out <- x$entries %>%
        tibble::enframe(name = "tmp", value = "tmp2") %>%
        tidyr::unnest_wider(col = "tmp2") %>%
        dplyr::select(-.data$tmp)

    # capture datetime when added to list
    get_stmt <- x$get_statements
    tbl_out <- tbl_out %>%
        dplyr::mutate(
            added = dplyr::first(get_stmt[names(get_stmt) == "date"])
        )

    tbl_out
}

#' @export
as_tibble.scopus_search_list <- function(x, ...) {
    purrr::map(x, as_tibble) %>%
        purrr::set_names(nm = names(x)) %>%
        dplyr::bind_rows(.id = "cites")
}
