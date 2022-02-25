#' Convert `esummary` Object into Tibble
#'
#' Converts an `esummary` object created by [pubmed_summary()] or
#' [rentrez::entrez_summary()] into a [tibble][tibble::tibble].
#'
#' @section Note:
#' For single inputs to [rentrez::entrez_summary()], `always_return_list` must
#' be `TRUE`).
#'
#' @param x An `esummary` object (`esummary_list` or `esummary_list_nested`)
#' @param ... Additional arguments. Not used.
#'
#' @return
#' A tibble, where `esummary_id` is the input identifier. `esummary_list_nested`
#' objects will additionally have a `cites` column.
#'
#' @export
as_tibble.esummary_list <- function(x, ...) {
    x %>%
        purrr::map(~ `class<-`(.x, "list")) %>% # strip esummary class, req'd t::uw
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")
}

#' @rdname as_tibble.esummary_list
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
