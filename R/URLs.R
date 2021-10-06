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
