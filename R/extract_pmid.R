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

    pmid_missing <- is.na(pmids)
    if (any(pmid_missing)) {
        n_missing <- sum(pmid_missing)
        n_id <- length(pmids)
        pct_missing <- round(n_missing / n_id, 2)

        warning(
            n_missing, " of ", n_id, " (", pct_missing, "%)",
            " PMIDs are missing. Consider extracting PMCIDs.",
            call. = FALSE
        )
    }

    pmids
}

#' @export
extract_pmid.data.frame <- function(x) {
    df <- dplyr::rename_with(x, .fn = tolower)

    if (!"pmid" %in% names(df)) {
        stop("PMID column could not be identified. Name must be 'pmid' or 'PMID').")
    }

    pmids <- df$pmid

    pmid_missing <- is.na(pmids)
    if (any(pmid_missing)) {
        n_missing <- sum(pmid_missing)
        n_id <- length(pmids)
        pct_missing <- round(n_missing / n_id, 2)

        warning(
            n_missing, " of ", n_id, " (", pct_missing, "%)",
            " PMIDs are missing. Consider extracting alternate IDs, if available.",
            call. = FALSE
        )
    }

    pmids
}
