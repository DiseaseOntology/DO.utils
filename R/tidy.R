# to make where available, since it's not exported from tidyselect yet
#   will be in next version > 1.1.2
utils::globalVariables("where")


#' Tidy SPARQL Query
#'
#' Tidies SPARQL query results, unnesting list columns, returning results as
#' a [tibble](tibble::tibble()) instead of a data.frame and, optionally,
#' converting URIs to CURIEs.
#'
#' @param query_res The results of a SPARQL query, as a data.frame (usually
#'     produced by [owl_xml()]$query() or similar from [DOrepo()].
#' @param as_curies Whether to convert IRIs to CURIEs, as a boolean
#'     (default: `TRUE`).
#' @inheritDotParams to_curie -x
#'
#' @section Note:
#' This function exists because the results are not currently tidied by `pyDOID`
#' (the python package that provides SPARQL query functionality to `DO.utils`).
#'
#' @export
tidy_sparql <- function(query_res, as_curies = TRUE, ...) {
    res <- query_res %>%
        tibble::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE)

    if (as_curies) {
        res <- res %>%
            dplyr::mutate(
                dplyr::across(dplyr::everything(), ~ to_curie(.x, ...))
            )
    }

    res
}


#' Tidy Publication Records
#'
#' Tidy publication record results into a tibble. Limited data is retained. If
#' full data retention is desired, consider using `as_tibble()` instead (expect
#' list columns and untidy column names).
#'
#' @param x A publication record object, as produced by `citedby_pubmed()`,
#' `citedby_scopus()` and `pubmed_summary()`.
#' @param ... Ignored; included for extensibility.
#'
#' @export
tidy_pub_records <- function(x, ...) {
    UseMethod("tidy_pub_records")
}

#' @export
tidy_pub_records.scopus_search_list <- function(x, ...) {
    tidy_pub_records.scopus_search(x, ...)
}

#' @export
tidy_pub_records.scopus_search <- function(x, ...) {
    pub_tbl <- as_tibble(x)

    col_select <- c(
        "first_author", "title" = "dc:title",
        "journal" = "prism:publicationName", "pub_date", "doi" = "prism:doi",
        "pmid" = "pubmed-id", "scopus_eid" = "eid", "pub_type", "added_dt"
    )
    col_collapse <- list(added_dt = "first")
    if ("cites" %in% names(pub_tbl)) {
        col_select <- append(
            col_select,
            "cites",
            after = which(col_select == "eid"))
        col_collapse <- append(col_collapse, c(cites = "unique"))
    }

    pub_tidy <- pub_tbl %>%
        dplyr::mutate(
            first_author = stringr::str_remove_all(.data$`dc:creator`, "\\."),
            pub_type = paste(
                .data$`prism:aggregationType`,
                .data$subtypeDescription,
                sep = "|"
            ),
            pub_date = lubridate::date(.data$`prism:coverDate`)
        ) %>%
        dplyr::select(!!!col_select) %>%
        collapse_col_flex(!!!col_collapse)

    pub_tidy
}

#' @export
tidy_pub_records.esummary_list_nested <- function(x, ...) {
    tidy_pub_records.esummary_list(x, ...)
}

#' @export
tidy_pub_records.esummary_list <- function(x, ...) {
    pub_tbl <- as_tibble(x)

    col_select <- c(
        "first_author" = "SortFirstAuthor", "title" = "Title",
        "journal" = "Source", "pub_date", "doi", "pmid", "pmcid", "pub_type",
        "added_dt"
    )
    col_collapse <- list(added_dt = "first")
    if ("cites" %in% names(pub_tbl)) {
        col_select <- append(
            col_select,
            "cites",
            after = which(col_select == "pmcid"))
        col_collapse <- append(col_collapse, c(cites = "unique"))
    }

    pub_tidy <- pub_tbl %>%
        hoist_ArticleIds() %>%
        dplyr::mutate(
            pub_type = purrr::map_chr(
                .data$PubType,
                vctr_to_string,
                delim = "|"
            ),
            pub_date = lubridate::date(.data$SortPubDate)
        ) %>%
            dplyr::select(!!!col_select) %>%
            collapse_col_flex(!!!col_collapse)

    pub_tidy
}
