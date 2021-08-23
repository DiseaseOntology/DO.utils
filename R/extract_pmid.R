#' Extract PubMed ID
#'
#' `extract_pmid` is a generic function that extracts a vector of PubMed IDs from
#' Entrez Utilities API results obtained via the [`rentrez`][rentrez::rentrez]
#' package.
#'
#' @param x rentrez API result
#'
#' @export
extract_pmid <- function(x, ...) {
    UseMethod("extract_pmid")
}

#' @export
extract_pmid.pm_search <- function(x) {
    x$ids
}

#' @export
extract_pmid.pmc_search <- function(x) {
    pmids <- x$pmids

    if (length(pmids) == 0) {
        stop("No PMIDs available. Was 'pmid' set to TRUE in search_pmc()?")
    }

    if (any(is.na(pmids))) {
        warning("Not all PMIDs were available. Consider using PMCIDs instead")
    }

    pmids
}
