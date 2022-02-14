#' Get Cited By List from PubMed
#'
#' List PubMed IDs of publications citing those specified by `web_history` or
#' `id`, potentially split `by_id`. `citedby_pmid()` can handle cases where the
#' number of input IDs is >200 automatically (unlike [rentrez::entrez_link]).
#'
#' @inheritParams rentrez::entrez_link
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

    cited_by
}


#' Get Cited By List from Scopus
#'
#' List Scopus publications citing those specified by `title` or `eid`,
#' potentially split `by_id`.
#'
#' @param title a character vector of publication titles to be used in a
#'     Scopus Search API REFTITLE() query; see
#'     https://dev.elsevier.com/sc_search_tips.html.
#' @param by_id a logical scalar where TRUE (default) splits cited by
#'     publications by the input title they cite, and FALSE returns the unified
#'     list of unique cited by publications.
#' @param id a vector of unique IDs with length equal to title; _REQUIRED_ if
#'      `by_id = TRUE`, otherwise ignored.
#' @inheritParams rscopus::scopus_search
#' @param headers additional headers to be added to [httr::add_headers()]
#' @param ... Arguments to be passed to the query list for [httr::GET()]
#'
#' @return If `by_id = FALSE`, the list result from the Scopus Search API (as
#'     produced by [rscopus::scopus_search]). If `by_id = TRUE`, an `id` named
#'     list of `scopus_search` result lists.
#'
#' @export
citedby_scopus <- function(title, by_id = FALSE, id = NULL,
                           api_key = NULL, view = "STANDARD",
                           start = 0, count = NULL, max_count = 20000,
                           headers = NULL, wait_time = 0, verbose = TRUE, ...) {

    assert_character(title)
    assert_scalar_logical(by_id)

    # if count unspecified, set to max count allowed for specified view
    #   this is just pagination (not sure why it can be user-specified),
    #   max_count controls the max number of records returned
    if (is.null(count)) {
        count <- switch(view, STANDARD = 200, COMPLETE = 25)
    }

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
        class(cited_by) <- ss_list_class
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
    }

    cited_by
}

merge_citedby <- function() { }
