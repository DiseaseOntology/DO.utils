#' List Available PubMed-to-PubMed Links
#'
#' List link types available for PubMed in the Entrez API that could be
#' specified in [rentrez::entrez_link()].
#'
#' @export
pubmed_links <- function() {
    pm_links <- entrez_link_info(db = "pubmed")
    pm_pm_links <- dplyr::filter(pm_links, DbTo == "pubmed")

    pm_pm_links
}


#' List Possible Links
#'
#' List possible database-to-database links provided by Entrez API (via
#' [rentrez](rentrez::rentrez)).
#'
#' Link names can be used in [rentrez::entrez_link()] with the `linkname`
#' argument.
#'
#' @inheritParams rentrez::entrez_info
#'
#' @return A tibble of Entrez API links
#'
#' @noRd
entrez_link_info <- function(db = NULL, config = NULL) {
    links_xml <- rentrez::entrez_info(db = db, config = config)
    links_list <- XML::xmlToList(links_xml)$DbInfo$LinkList
    links_tidy <- dplyr::bind_rows(links_list)

    links_tidy
}
