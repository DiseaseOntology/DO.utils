#' Get Cited By List from Scopus
#'
#' List Scopus publications citing those specified by `title`, optionally split
#' `by_id`.
#'
#' @param title Publication title(s) to be used in a Scopus Search API
#'     REFTITLE() query, as a character vector; see
#'     https://dev.elsevier.com/sc_search_tips.html.
#' @inheritParams citedby_pubmed
#' @param id Unique IDs used to identify `title` input, as a character vector of
#'     the same length as `title`; _REQUIRED_ if `by_id = TRUE`, otherwise
#'     ignored.
#' @inheritParams rscopus::scopus_search
#' @param headers Additional headers to be added by [httr::add_headers()].
#' @param ... Named arguments to be passed to [httr::GET()]. Available arguments
#'     are listed in the
#'     [Scopus Search Documentation: API Specification](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl).
#'
#' @return If `by_id = FALSE`, the list result from the Scopus Search API (as
#'     produced by [rscopus::scopus_search]). If `by_id = TRUE`, an `id` named
#'     list of `scopus_search` result lists.
#'
#' @family citedby_functions
#' @export
citedby_scopus <- function(title, by_id = FALSE, id = NULL,
                           api_key = NULL, view = "STANDARD", save_raw = NULL,
                           overwrite = FALSE, start = 0, count = NULL,
                           max_count = 20000, headers = NULL, wait_time = 0,
                           verbose = TRUE, ...) {
    certify_save_raw(save_raw, overwrite)
    assert_character(title)
    assert_scalar_logical(by_id)

    # if count unspecified, set to max count allowed for specified view
    #   this is just pagination (not sure why it can be user-specified),
    #   max_count controls the max number of records returned
    if (is.null(count)) {
        count <- switch(view, STANDARD = 200, COMPLETE = 25)
    }

    if (by_id) {
        assert_character(id)
        assertthat::assert_that(length(title) == length(id))

        citedby_raw <- purrr::map(
            title,
            function(t) {
                q <- scopus_title_query(t)
                res <- rscopus::scopus_search(
                    query = q,
                    api_key = api_key,
                    view = view,
                    start = start,
                    count = count,
                    max_count = max_count,
                    headers = headers,
                    wait_time = wait_time,
                    verbose = verbose,
                    ...
                )
                class(res) <- ss_class
                res
            }
        )
        names(cited_by) <- id
        class(cited_by) <- ss_list_class
    } else {
        q <- scopus_title_query(title)
        citedby_raw <- rscopus::scopus_search(
            query = q,
            api_key = api_key,
            view = view,
            start = start,
            count = count,
            max_count = max_count,
            headers = headers,
            wait_time = wait_time,
            verbose = verbose,
            ...
        )
        class(cited_by) <- ss_class
    }

    if (!is.null(save_raw)) {
        save(citedby_raw, file = save_raw)
    }

    citedby <- as_tibble(citedby_raw) %>%
        dplyr::mutate(
            first_author = stringr::str_remove_all(`dc:creator`, "\\."),
            pub_type = paste(
                `prism:aggregationType`,
                subtypeDescription,
                sep = "|"
            ),
            pub_date = lubridate::date(`prism:coverDate`),
        ) %>%
        dplyr::mutate(source = "scopus")


    if (by_id) {
        citedby <- citedby %>%
            dplyr::select(
            first_author, title = "dc:title", journal = "prism:publicationName",
            pub_date, doi = "prism:doi", pmid = "pubmed-id", scopus_eid = eid,
            cites, pub_type, added_dt = added
        ) %>%
        collapse_col_flex(cites = "unique", added_dt = "first")
    } else {
        citedby <- citedby %>%
            dplyr::select(
                first_author, title = "dc:title", journal = "prism:publicationName",
                pub_date, doi = "prism:doi", pmid = "pubmed-id", scopus_eid = eid,
                pub_type, added_dt = added
            ) %>%
            collapse_col_flex(added_dt = "first")
    }

    class(citedby) <- c("scopus_citedby", "citation_df", class(citedby))
    citedby
}


#' Get Cited By List from PubMed
#'
#' List PubMed publications citing those specified by `id`, optionally split
#' `by_id`.
#'
#' @inheritParams citedby_pmid
#' @inheritParams extract_pmid.elink_list
#' @inheritParams pubmed_summary
#' @param save_raw The `.rda` file path for saving the unprocessed citation
#'     response, as a string. Not saved if `NULL` (default).
#' @param overwrite Whether an existing unprocessed citation response should be
#'     overwritten; one of `TRUE` or `FALSE` (default).
#'
#' @family citedby_functions
#' @export
citedby_pubmed <- function(id = NULL, web_history = NULL, by_id = FALSE,
                           no_result = "warning", save_raw = NULL,
                           overwrite = FALSE, config = NULL, version = "2.0",
                           retmode = "xml", ...) {
    certify_save_raw(save_raw, overwrite)

    pmid_raw <- citedby_pmid(
        id = id,
        web_history = web_history,
        by_id = by_id,
        config = config,
        ...
    )

    pmid <- extract_pmid(
        pmid_raw,
        no_result = no_result,
        linkname = "pubmed_pubmed_citedin"
    )

    citedby_raw <- pubmed_summary(pmid, version = version, retmode = retmode)

    if (!is.null(save_raw)) {
        save(citedby_raw, file = save_raw)
    }

    citedby <- as_tibble(citedby_raw) %>%
        hoist_ArticleIds() %>%
        dplyr::mutate(
            pub_type = purrr::map_chr(PubType, vctr_to_string, delim = "|"),
            pub_date = lubridate::date(SortPubDate)
        ) %>%
        dplyr::mutate(source = "pubmed") %>%
        dplyr::mutate(added_dt = lubridate::now(tzone = "UTC"))

    include_cites <- by_id && if (!is.null(id)) {
        length(id) > 1
    } else {
        wh_id <- get_wh_id(web_history, "pubmed")
        length(wh_id) > 1
    }

    if (include_cites) {
        citedby <- citedby %>%
            dplyr::select(
                first_author = SortFirstAuthor, title = Title, journal = Source,
                pub_date, doi, pmid, pmcid, cites, pub_type
            ) %>%
            collapse_col(cites)
    } else {
        citedby <- citedby %>%
            dplyr::select(
                first_author = SortFirstAuthor, title = Title, journal = Source,
                pub_date, doi, pmid, pmcid, pub_type
            )
    }

    class(citedby) <- c("pubmed_citedby", "citation_df", class(citedby))
    citedby
}


#' Get Cited By PMIDs from PubMed
#'
#' List PubMed IDs of publications citing those specified by `web_history` or
#' `id`, potentially split `by_id`. `citedby_pmid()` can handle cases where the
#' number of input IDs is >200 automatically (unlike [rentrez::entrez_link]).
#'
#' @param id PubMed IDs for which to retrieve "cited by", as a character vector.
#' @param web_history An Entrez History object to be used instead of `id`, as
#'     generated by the modified [rentrez::entrez_post()] fork at
#'     https://github.com/allenbaron/rentrez (see NOTE); should be used when
#'     "cited by" information is desired for >200 IDs.
#' @param by_id Whether to split "cited by" publications by the ID they
#'     cite, as a boolean (default: `TRUE`); `FALSE` returns a unified list of
#'     _unique_ cited by publications _without_ reference to the ID they cite.
#' @param config Configuration options passed to [httr::GET()].

#' @param ... Additional terms to add to the request, see NCBI documentation on
#'     [ESearch](https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_ESearch_)
#'     for a complete list.
#'
#' @section NOTE:
#' The rentrez fork at https://github.com/allenbaron/rentrez implements pull
#' request [#174](https://github.com/ropensci/rentrez/pull/174), which is not
#' yet implemented in the CRAN rentrez and is required for
#' rentrez::entrez_post() to function fully.
#'
#' @export
citedby_pmid <- function(id = NULL, web_history = NULL, by_id = FALSE,
                         config = NULL, save_raw = NULL, overwrite = FALSE,
                         ...) {

    if (is.null(web_history) & length(id) > 200) {
        web_history <- rentrez::entrez_post("pubmed", id = id)
        id <- NULL
    }

    cited_by <- rentrez::entrez_link(
        id = id,
        web_history = web_history,
        by_id = by_id,
        config = config,
        dbfrom = "pubmed",
        db = "pubmed",
        cmd = "neighbor",
        linkname = "pubmed_pubmed_citedin",
        ...
    )
    if (by_id) {
        names(cited_by) <- id
    }

    cited_by
}


# citedby_*() helpers -----------------------------------------------------

certify_save_raw <- function(save_raw = NULL, overwrite = FALSE) {
    if (!is.null(save_raw)) {
        if (tools::file_ext(save_raw) != "rda") {
            rlang::abort(
                c(
                    "`save_raw` must use the '.rda' file extension.",
                    x = save_raw
                )
            )
        }
        if (file.exists(save_raw)) {
            if (!overwrite) {
                rlang::abort(
                    paste0(
                        "Raw data file '", save_raw, "'",
                        " exists. Use `overwrite = TRUE` to replace it."
                    ),
                    .frame = parent.frame()
                )
            }
        } else if (!dir.exists(dirname(save_raw))) {
            rlang::abort(
                c(
                    paste0("Cannot create '", save_raw, "'."),
                    x = paste0(
                        "Directory '", dirname(save_raw),
                        "' does not exist."
                    )
                ),
                .frame = parent.frame()
            )
        }
    }

    invisible(TRUE)
}


get_wh_id <- function(web_history, db, ...) {
    id_string <- rentrez::entrez_fetch(
        db = db,
        web_history = web_history,
        rettype = "uilist",
        ...
    )

    id <- stringr::str_split(id_string, "\n") %>%
        unlist() %>%
        drop_blank()

    id
}
