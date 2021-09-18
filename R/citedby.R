#' Gets Cited By List from PubMed
#'
#' Lists the publications citing those specified by `web_history` or `id`,
#' potentially split `by_id`. `pubmed_citedby` can handle cases where the
#' number of input IDs is >200.
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
        linkname = "pubmed_pubmed_citedin"
    )

    cited_by
}
