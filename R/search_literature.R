#' Search PubMed
#'
#' Searches PubMed using Entrez Utilities API via [`rentrez`][rentrez::rentrez].
#'
#' @param term the search term (character); see reference or
#' [rentrez::entrez_search] for syntax details
#' @inheritParams rentrez::entrez_search
#' @param retmax maximum number of PubMed IDs to return (integer; default = 20,
#' max = 100,000); see reference under 'Optional Parameters – Retrieval'
#'
#' @seealso rentrez::entrez_search
#'
#' @references
#' https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_ESearch_
#'
#' @export
search_pubmed <- function(term, config = NULL, retmode = "xml",
                          use_history = FALSE, retmax = NULL, ...) {

    # prevent R conversion of retmax to scientific notation, scheduled to be
    #   fixed in rentrez v1.2.4
    retmax <- if (!is.null(retmax)) as.integer(retmax)

    pm_res <- rentrez::entrez_search(
        db = "pubmed",
        term = term,
        config = config,
        retmode = retmode,
        use_history = use_history,
        retmax = retmax,
        ...
    )

    class(pm_res) <- append("pm_search", class(pm_res))
    pm_res
}


#' Search PubMed Central
#'
#' Searches PubMed Central using Entrez Utilities API via
#' [`rentrez`][rentrez::rentrez].
#'
#' @inheritParams search_pubmed
#' @param pmid whether to return PMID as well as PMCID (default = FALSE);
#' when TRUE, PMIDs are obtained via [rcrossref::id_converter])
#'
#' @export
search_pmc <- function(term, config = NULL, retmode = "xml",
                       use_history = FALSE, pmid = FALSE, retmax = NULL, ...) {

    # prevent R conversion of retmax to scientific notation, scheduled to be
    #   fixed in rentrez v1.2.4
    retmax <- if (!is.null(retmax)) as.integer(retmax)

    pmc_res <- rentrez::entrez_search(
        db = "pmc",
        term = term,
        config = config,
        retmode = retmode,
        use_history = use_history,
        retmax = retmax,
        ...
    )

    # Add PMC to complete full ID - output is only numeric portion of PMCID;
    #   https://www.ncbi.nlm.nih.gov/labs/pmc/tools/get-pmcids/)
    if (!rlang::is_empty(pmc_res$ids)) {
        pmc_res$ids <- paste0("PMC", pmc_res$ids)
    }

    class(pmc_res) <- append("pmc_search", class(pmc_res))

    if (isTRUE(pmid)) {
        if (rlang::is_empty(pmc_res$ids)) {
            pmc_res$pmids <- NULL
        } else {
            all_ids <- batch_id_converter(pmc_res$ids)
            pmc_res$pmids <- extract_pmid(all_ids)
        }
    }

    pmc_res
}
