#' Convert `esummary` Object into Tibble
#'
#' Converts an `esummary` object into a [tibble][tibble::tibble].
#'
#' @section Note:
#' For single inputs to [rentrez::entrez_summary()], `always_return_list` must
#' be `TRUE`.
#'
#' @param x An `esummary` object (`esummary_list` or `esummary_list_nested`)
#' @param ... Ignored; included for extensibility.
#'
#' @return
#' An untidy, where `esummary_id` is the input identifier. `esummary_list_nested`
#' objects will have an additional `cites` column.
#'
#' @seealso citedby_pubmed, pubmed_summary
#' @export
as_tibble.esummary_list <- function(x, ...) {
    tbl_out <- x %>%
        # strip esummary class, required workaround for tidyr::unnest_wider
        #   see https://github.com/tidyverse/tidyr/issues/1327
        purrr::map(~ `class<-`(.x, "list")) %>%
        tibble::enframe(name = "esummary_id", value = "tmp") %>%
        tidyr::unnest_wider(col = "tmp")

    # ensure consistently formatted list-columns
    list_cols <- c("Authors", "Lang", "PubType", "ArticleIds", "History",
                   "References", "Attributes")
        # 2022-11-10 PMC results no longer have all these columns
    cols_in_tbl <- list_cols[list_cols %in% names(tbl_out)]
    list_in_tbl <- names(tbl_out)[purrr::map_lgl(tbl_out, rlang::is_list)]
    reformat_col <- cols_in_tbl[!cols_in_tbl %in% list_in_tbl]

    if (length(reformat_col) > 0) {
        tbl_out <- dplyr::mutate(
            tbl_out,
            dplyr::across(
                dplyr::one_of(reformat_col),
                as.list
            )
        )
    }

    tbl_out <- dplyr::mutate(tbl_out, added_dt = lubridate::now(tzone = "UTC"))
    tbl_out
}

#' @rdname as_tibble.esummary_list
#' @export
as_tibble.esummary_list_nested <- function(x, ...) {
    purrr::map(x, as_tibble.esummary_list) %>%
        dplyr::bind_rows(.id = "cites")
}

#' Convert `scopus_search` Object into Tibble
#'
#' Converts a `scopus_search` object into a [tibble][tibble::tibble].
#'
#' @param x A `scopus_search` or `scopus_search_list` object.
#' @param ... Ignored; included for extensibility.
#'
#' @return
#' An untidy tibble. `scopus_search_list` objects will have an additional
#' `cites` column.
#'
#' @seealso citedby_scopus
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
            added_dt = dplyr::first(get_stmt[names(get_stmt) == "date"])
        )

    tbl_out
}

#' @rdname as_tibble.scopus_search
#' @export
as_tibble.scopus_search_list <- function(x, ...) {
    purrr::map(x, as_tibble) %>%
        purrr::set_names(nm = names(x)) %>%
        dplyr::bind_rows(.id = "cites")
}
