#' Get URL (internal)
#'
#' Get a URL used within this package. Available URLs: "doi", "pubmed", "pmc",
#' "pmc_article" (append as prefix to article pmcid for direct navigation),
#' "alliance_disease_tsv"
#'
#' @param .name internal name of desired URL
get_url <- function(.name) {
    .name <- match.arg(
        .name,
        c("doi", "pubmed", "pmc", "pmc_article", "alliance_disease_tsv")
    )

    switch(
        .name,
        doi = "https://www.doi.org/",
        pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
        pmc = "https://www.ncbi.nlm.nih.gov/pmc/",
        pmc_article = "https://www.ncbi.nlm.nih.gov/pmc/articles/",
        alliance_disease_tsv = "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
    )
}

#' Append to URL
#'
#' Append a value to a URL.
#'
#' @section Note:
#' No URL validation is performed.
#'
#' @param x value to append
#' @param url a URL or the internal name of a URL used in this package (see
#' [get_url] for possible names)
#'
#' @export
append_to_url <- function(x, url) {

    url <- tryCatch(get_url(url), error = function(e) url)

    # add '/' if no terminal '/' in URL
    if (stringr::str_detect(url, "/$")) {
        new_url <- paste0(url, x)
    } else {
        new_url <- paste0(url, "/", x)
    }

    new_url
}
