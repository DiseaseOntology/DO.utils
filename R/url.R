# Ensure respectful URL testing -----------------------------------------------

get_delay <- function(robotstxt, .user_agent = pkg_user_agent,
                      default = NA_integer_) {

    cd_df <- robotstxt$crawl_delay
    if (nrow(cd_df) == 0) {
        return(default)
    }

    # delay for user agent
    delay <- dplyr::filter(cd_df, .data$useragent == .user_agent)$value
    if (rlang::is_empty(delay)) {
        delay <- dplyr::filter(cd_df, .data$useragent == "*")$value
    }

    # general delay
    if (rlang::is_empty(delay)) {
        delay <- default
    }

    as.integer(delay)
}


trim_url <- function(url_no_domain) {
    url_sep_count <- stringr::str_count(url_no_domain, "[&#]")
    replace_regex <- paste0("([^&#]*([&#].*){", url_sep_count - 1, "})[#&].*")
    trimmed <- purrr::pmap_chr(
        .l = list(u = url_no_domain, c = url_sep_count, r = replace_regex),
        function(u, c, r) {
            ifelse(
                c > 0,
                stringr::str_replace(u, r, "\\1"),
                dirname(u)
            )
        }
    )

    trimmed
}


# Build URLs for common domains -------------------------------------------

#' Get URL (internal)
#'
#' Get a URL used within this package. Available URLs: "doi", "github", "orcid",
#' "pubmed", "pmc", "pmc_article" (append as prefix to article pmcid for direct
#' navigation), or "alliance_disease_tsv".
#'
#' @param .name internal name of desired URL
#' @keywords internal
get_url <- function(.name) {
    base_url <- c(
        MESH = "https://meshb.nlm.nih.gov/record/ui?ui=",
        NCI = "https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=",
        UMLS_CUI = "https://uts.nlm.nih.gov/uts/umls/concept/",
        ICD10 = "http://www.icd10data.com/Search.aspx?search=",
        ICD9 = "http://icd9cm.chrisendres.com/index.php?action=search&srchtext=",
        SNOMEDCT_US = "https://browser.ihtsdotools.org/?perspective=full&conceptId1=",
        ORDO = "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?lng=en&Expert=",
        GARD = "https://rarediseases.info.nih.gov/diseases/{id}/index",
        OMIM = "https://omim.org/MIM:",
        EFO = "http://www.ebi.ac.uk/efo/",
        # ICDO, KEGG, ICD11, MEDDRA
        doi = "https://www.doi.org/",
        github = "https://github.com/",
        orcid = "https://orcid.org/",
        pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
        pmc = "https://www.ncbi.nlm.nih.gov/pmc/",
        pmc_article = "https://www.ncbi.nlm.nih.gov/pmc/articles/",
        alliance_disease_tsv = "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
    )

    if (.name == "names") {
        names(base_url)
    } else {
        base_url[[.name]]
    }
}
