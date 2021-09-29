#' Get Cited By List from PubMed
#'
#' List PubMed IDs of publications citing those specified by `web_history` or
#' `id`, potentially split `by_id`. `citedby_pubmed` can handle cases where the
#' number of input IDs is >200 automatically (unlike [rentrez::entrez_link]).
#'
#' @inheritParams rentrez::entrez_link
#' @export
citedby_pubmed <- function(id = NULL, web_history = NULL, by_id = FALSE,
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
#' @param title Scopus Search API REFTITLE() query; see
#' https://dev.elsevier.com/sc_search_tips.html
#' @param by_id logical; where FALSE (default) prescribes a single query
#' (multiple titles are separated by OR) and TRUE prescribes a separate query
#' for each title
#' @param id (optional) vector of unique IDs; _ignored if_ `by_id = FALSE`
#' @inheritParams rscopus::scopus_search
#'
#' @return A list; if `by_id = FALSE`, the list result from the Scopus Search
#' API as produced by [rscopus::scopus_search]; if `by_id = TRUE` a list of
#' `rscopus::scopus_search` list results labelled numerically or with `id`, if
#' specified.
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
        if (is.null(id)) {
            id <- seq_along(title)
        } else {
            assertthat::assert_that(length(title) == length(id))
        }

        cited_by <- purrr::map(
            title,
            function(t) {
                q <- scopus_title_query(t)
                res <- scopus_search(
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
            }
        )
        names(cited_by) <- id
        class(cited_by) <- ss_list_class
        } else {
            q <- scopus_title_query(title)
            cited_by <- scopus_search(
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
