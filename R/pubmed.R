#' Get PubMed Summary
#'
#' Retrieves PubMed Summary information for specified publications. Acts as a
#' simple wrapper around [rentrez::entrez_summary] to allow larger number of
#' IDs as input (using [rentrez::entrez_post]).
#'
#' @inheritParams rentrez::entrez_summary
#' @param version,always_return_list preferred defaults are set but
#' included here for flexibility; see [rentrez::entrez_summary()] for details
#' @param retmode "xml" (default) or "json"; "xml" is preferred because of a
#' higher limit. This default is opposite the [rentrez::entrez_summary()]
#' default.
#'
#' @export
pubmed_summary <- function(id = NULL, web_history = NULL, config = NULL,
                           version = "2.0", retmode = "xml",
                           always_return_list = TRUE, ...) {

    if (is.null(web_history) & length(id) > 200) {
        web_history <- rentrez::entrez_post("pubmed", id = id)
        id <- NULL
    }

    pm_summary <- rentrez::entrez_summary(
        db = "pubmed",
        id = id,
        web_history = web_history,
        version = version,
        always_return_list = always_return_list,
        retmode = retmode,
        config = config,
        ...
    )

    pm_summary
}
