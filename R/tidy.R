# to make where available, since it's not exported from tidyselect yet
#   will be in next version > 1.1.2
utils::globalVariables("where")


#' Tidy SPARQL Query
#'
#' Tidies SPARQL query results, unnesting list columns, removing `?` from column
#' names, and returning results as a [tibble][tibble::tibble()] instead of a
#' data.frame. Also optionally converts URIs to CURIEs and replaces `NA` in
#' logical outputs with `FALSE`.
#'
#' @param query_res The results of a SPARQL query, as a data.frame (usually
#'     produced by [owl_xml()]$query() or similar from [DOrepo()], but can also
#'     be used to tidy results of [robot("query", ...)][robot] loaded with
#'     `readr`).
#' @param as_curies Whether to convert IRIs to CURIEs, as a boolean
#'     (default: `TRUE`).
#' @param lgl_NA_false Whether to replace `NA` values with `FALSE` in logical
#'     outputs, as a boolean (default: `TRUE`).
#' @inheritDotParams to_curie -x
#'
#' @export
tidy_sparql <- function(query_res, as_curies = TRUE, lgl_NA_false = TRUE, ...) {
    res <- query_res %>%
        tibble::as_tibble() %>%
        DO.utils::unnest_cross(where(is.list), keep_empty = TRUE)
    names(res) <- stringr::str_remove(names(res), "^\\?")

    if (as_curies) {
        res <- res %>%
            dplyr::mutate(
                dplyr::across(dplyr::where(is.character), ~ to_curie(.x, ...))
            )
    }

    if (lgl_NA_false) {
        res <- res %>%
            dplyr::mutate(
                dplyr::across(dplyr::where(is.logical), ~ replace_na(.x, FALSE))
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


#' Tidy Google Analytics Tables (INTERNAL)
#'
#' Tidies Google Analytics tables loaded with [read_ga()].
#'
#' @param ga_tbl A Google analytics table loaded with [read_ga()], that needs
#'     to be tidied.
#' @param keep_total Whether to keep the row with totals that appears at the
#'     bottom of the table, as a logical scalar (default: `FALSE`).
#'
#' @keywords internal
tidy_ga_tbl <- function(ga_tbl, keep_total = FALSE) {
    stopifnot(length(keep_total) == 1, is.logical(keep_total))
    rlang::check_installed(
        pkg = c("dplyr", "stringr", "tidyr", "lubridate"),
        reason = "to use tidy_ga_tbl()"
    )

    out <- ga_tbl %>%
        dplyr::rename_with(
            .fn = ~ stringr::str_to_lower(.x) %>%
                stringr::str_replace_all(
                    c("/" = "per", "[. ]+" = "_")
                )
        )

    if ("date_range" %in% names(out)) {
        out <- out %>%
            tidyr::separate_wider_delim(
                cols = .data$date_range,
                delim = stringr::regex(" *- *"),
                names = c("date_start", "date_end"),
                cols_remove = TRUE
            )
    }

    out <- out %>%
        dplyr::rename_with(
            .cols = dplyr::where(
                ~ is.character(.x) && any(stringr::str_detect(.x, "%$"), na.rm = TRUE)
            ),
            .fn = ~ paste0(.x, "_pct", recycle0 = TRUE)
        ) %>%
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::matches("^(day|date)_"),
                .fns = ~ lubridate::mdy(.x)
            ),
            dplyr::across(
                .cols = dplyr::where(is.character) & dplyr::ends_with("_pct"),
                .fns = ~ as.numeric(stringr::str_remove(.x, "%$"))
            ),
            dplyr::across(
                .cols = dplyr::ends_with("_duration"),
                .fns = ~ readr::parse_time(stringr::str_remove(.x, "^<"))
            )
        )

    if (!keep_total) {
        out <- out %>%
            dplyr::filter(dplyr::if_all(.cols = 1, .fns = ~!is.na(.x)))
    } else {
        out <- out %>%
            dplyr::mutate(
                dplyr::across(.cols = 1, ~ dplyr::if_else(is.na(.x), "TOTAL", .x))
            )
    }

    out
}
