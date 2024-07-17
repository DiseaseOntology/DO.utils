# to make where available, since it's not exported from tidyselect yet
#   will be in next version > 1.1.2
utils::globalVariables("where")


#' Tidy SPARQL Query
#'
#' Tidies SPARQL query results according to desired specifications (see
#' `tidy_what` parameter for details).
#'
#' @param query_res The results of a SPARQL query, as a data.frame (usually
#'     produced by [owl_xml()]$query() or similar from [DOrepo()], but can also
#'     be used to tidy results of [robot("query", ...)][robot] loaded with
#'     `readr`).
#' @param tidy_what The elements of a SPARQL-created data.frame to tidy, as a
#' character vector. One or more of the following:
#'
#' * `"everything"` to apply all tidy operations (has precedence over
#' `"nothing"`).
#' * `"header"` to remove leading `?` from header labels.
#' * `"unnest"` to unnest list columns with [unnest_cross()].
#' * `"uri_to_curie"` to convert all URIs recognized by DO.utils to CURIEs with
#' [to_curie()].
#' * `"lgl_NA_false"` to replace `NA` in logical columns with `FALSE`.
#' * `"as_tibble"` to make the output a [tibble][tibble::tibble].
#' * `"rm_lang_tag"` to remove language tags. Tags will only be removed from
#' `character` class columns, and then only if there is one unique language tag
#' in the given column.
#' * `"nothing"` to prevent all tidying.
#' @inheritDotParams to_curie -x
#'
#' @export
tidy_sparql <- function(query_res, tidy_what = "everything", ...) {
    what_opts <- c("header", "unnest", "uri_to_curie", "lgl_NA_FALSE",
                          "as_tibble", "rm_lang_tag")
    tidy_what <- match.arg(
        tidy_what,
        choices = c(what_opts, "everything", "nothing"),
        several.ok = TRUE
    )
    if ("everything" %in% tidy_what) tidy_what <- what_opts
    if ("nothing" %in% tidy_what) return(query_res)

    res <- query_res
    if ("header" %in% tidy_what) {
        names(res) <- stringr::str_remove(names(res), "^\\?")
    }

    if ("unnest" %in% tidy_what) {
        res <- DO.utils::unnest_cross(res, where(is.list), keep_empty = TRUE)
    }

    if ("uri_to_curie" %in% tidy_what) {
        res <- dplyr::mutate(
            res,
            dplyr::across(dplyr::where(is.character), ~ to_curie(.x, ...))
        )
    }

    if ("lgl_NA_FALSE" %in% tidy_what) {
        res <- dplyr::mutate(
            res,
            dplyr::across(dplyr::where(is.logical), ~ replace_na(.x, FALSE))
        )
    }

    if ("as_tibble" %in% tidy_what) {
        res <- tibble::as_tibble(res)
    }

    if ("rm_lang_tag" %in% tidy_what) {
        col_tags <- purrr::map(
            res,
            function(.x) {
                if (!is.character(.x)) return(character(0))
                stringr::str_extract_all(.x, "@[a-z]{2}") %>%
                    unlist() %>%
                    na.omit() %>%
                    unique()
            }
        )
        mult_tag <- purrr::map_lgl(col_tags, ~ length(.x) > 1)

        if (any(mult_tag)) {
            mt_col <- names(col_tags)[mult_tag]
            col_tag_cat <- purrr::map_chr(
                col_tags[mult_tag],
                ~  trunc_cat_n(
                    stringr::str_remove(.x, "^@"),
                    5
                )
            )
            col_msg <- paste0(mt_col, ": ", col_tag_cat)
            rlang::inform(
                c(
                    "Multiple language tags were found and retained in the following columns:",
                    purrr::set_names(col_msg, rep("i", length(col_msg)))
                )
            )
        }

        res <- dplyr::mutate(
            res,
            dplyr::across(
                names(col_tags)[!mult_tag],
                ~ stringr::str_remove(.x, "@[a-z]{2}")
            )
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
