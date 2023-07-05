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
#' @param insttoken Elsevier institutional token (_REQUIRED_). See
#'     `vignette("obtain_use_records")` for more details.
#' @param no_result The type of condition that should be signaled when no Scopus
#'     results exist in a response; one of "error", "warning" (default),
#'     "message" or "none".
#' @param ... Named arguments to be passed to [httr::GET()]. Available arguments
#'     are listed in the
#'     [Scopus Search Documentation: API Specification](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl).
#' @param headers Additional headers to be added by [httr::add_headers()].
#'
#' @return If `by_id = FALSE`, the list result from the Scopus Search API (as
#'     produced by [rscopus::scopus_search]). If `by_id = TRUE`, an `id` named
#'     list of `scopus_search` result lists.
#'
#' @family citedby_functions
#' @export
citedby_scopus <- function(title, by_id = FALSE, id = NULL, api_key = NULL,
                           insttoken = NULL, no_result = "warning",
                           view = "STANDARD", start = 0,
                           count = NULL, max_count = 20000, headers = NULL,
                           wait_time = 0, verbose = FALSE, ...) {
    assert_character(title)
    assert_scalar_logical(by_id)
    no_result <- match.arg(
        no_result,
        c("error", "warning", "message", "none")
    )
    no_res_msg <- "0 Scopus citedby results"
    # if count unspecified, set to max count allowed for specified view
    #   this is just pagination (not sure why it can be user-specified),
    #   max_count controls the max number of records returned
    if (is.null(count)) {
        count <- switch(view, STANDARD = 200, COMPLETE = 25)
    }
    headers <- use_scopus_insttoken(insttoken, headers)

    if (by_id && length(title) > 1) {
        assert_character(id)
        assertthat::assert_that(length(title) == length(id))

        cited_by <- purrr::map(
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

        no_res <- purrr::map_lgl(cited_by, ~ .x$total_results == 0)
        if (any(no_res)) {
            discard <- id[no_res]
            # preserve Scopus API responses when discarded
            tmp <- cited_by[!no_res]
            attr(tmp, "discarded_response") <- purrr::map(
                cited_by[discard],
                ~ .x$get_statements
            )
            cited_by <- tmp

            if (no_result != "none") {
                rlang::signal(
                    message = c(
                        paste0("Discarded (", no_res_msg, ")"),
                        purrr::set_names(discard, rep("i", length(discard)))
                    ),
                    class = c("no_result", no_result),
                    use_cli_format = TRUE
                )
            }
        }
        if (length(cited_by) != 0) class(cited_by) <- ss_list_class
    } else {
        q <- scopus_title_query(title)
        cited_by <- rscopus::scopus_search(
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

        if (cited_by$total_results == 0) {
            if (no_result != "none") {
                rlang::signal(
                    message = c(no_res_msg, i = title),
                    class = c("no_result", no_result),
                    use_cli_format = TRUE
                )
            }

            # preserve Scopus API responses when discarded
            tmp <- list()
            attr(tmp, "discarded_response") <- cited_by$get_statements
            cited_by <- tmp
        }
    }

    cited_by
}


#' Get Cited By List from PubMed
#'
#' List PubMed publications citing those specified by `id`, optionally split
#' `by_id`.
#'
#' @inheritParams citedby_pmid
#' @inheritParams extract_pmid.elink_list
#' @inheritParams pubmed_summary
#'
#' @family citedby_functions
#' @export
citedby_pubmed <- function(id = NULL, web_history = NULL, by_id = FALSE,
                           no_result = "warning", config = NULL,
                           version = "2.0", retmode = "xml", ...) {

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

    citedby <- pubmed_summary(pmid, version = version, retmode = retmode)

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
                         config = NULL, ...) {

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
