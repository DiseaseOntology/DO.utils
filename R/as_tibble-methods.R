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
    df <- x %>%
        # strip esummary class, required for tidyr::unnest_wider
        purrr::map(~ `class<-`(.x, "list")) %>%
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")

    # ensure consistently formatted list-columns
    list_cols <- c("Authors", "Lang", "PubType", "ArticleIds", "History",
                   "References", "Attributes")
    list_in_df <- names(df)[purrr::map_lgl(df, rlang::is_list)]
    not_list <- list_cols[!list_cols %in% list_in_df]
    if (length(not_list) > 0) {
        df <- dplyr::mutate(
            df,
            dplyr::across(
                dplyr::one_of(not_list),
                as.list
            )
        )
    }

    df
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
