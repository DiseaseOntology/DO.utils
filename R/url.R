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
#' Get a URL used within this package. To see all possible names, organized by
#' type use `get_url("names")`.
#'
#' @section NOTE:
#' `get_url()` provides prefixes for disease-ontology.org style link support.
#' The prefixes are primarily for cross-references (`xrefs`) and are generally
#' better for creating URLs to look up information about a particular entity
#' online but they _may_ overlap with official namespaces in [ns_prefix], which
#' can also be accessed by `get_url()`. Where this occurs, the prefixes can
#' generally be distinguished by capitalization, with xref prefixes in
#' uppercase, e.g.`r paste0('\ua0', '"MESH"', '\ua0=\ua0"', get_url("MESH"), '"')`,
#' and the official namespace in lowercase,
#' e.g.`r paste0('\ua0', '"mesh"', '\ua0=\ua0"', get_url("mesh"), '"')`. As this
#' distinction cannot always be guaranteed, care should be taken for any prefix
#' related to xrefs in the DO.
#'
#' @param .name Internal name of desired URL.
#' @keywords internal
get_url <- function(.name) {
    if (is.na(.name)) return(.name)
    base_url <- list(
        pub_urls = c(
            doi = "https://www.doi.org/",
            github = "https://github.com/",
            orcid = "https://orcid.org/",
            pubmed = "https://pubmed.ncbi.nlm.nih.gov/",
            pmc = "https://www.ncbi.nlm.nih.gov/pmc/"
        ),
        xref_urls = c(
            # GARD = "https://rarediseases.info.nih.gov/diseases/{LUI}/index", # requires glue spec, exclude for now
            ICD9CM = "http://icd9cm.chrisendres.com/index.php?action=search&srchtext=",
            ICD10CM = "http://www.icd10data.com/Search.aspx?search=",
            # ICD11 = "http://id.who.int/icd/entity/", # uses numerical identifier, e.g. 1711526170, not code
            # ICDO, # no browser
            KEGG = "https://www.kegg.jp/pathway/hsa", # to disease pathway (consistent w/DO), not disease itself (H# -> disease)
            # MEDDRA, # no browser
            MESH = "https://meshb.nlm.nih.gov/record/ui?ui=",
            NCI = "https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=",
            ORDO = "https://www.orpha.net/consor/cgi-bin/OC_Exp.php?lng=en&Expert=",
            OMIM = "http://www.omim.org/MIM:",
            SNOMEDCT_US = "https://browser.ihtsdotools.org/?perspective=full&conceptId1=",
            UMLS_CUI = "https://uts.nlm.nih.gov/uts/umls/concept/"
        ),
        web = c(
            DO_website = "https://disease-ontology.org/?id="
        ),
        data_urls = c(
            alliance_disease_tsv = "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
        ),
        ns_prefix = ns_prefix
    )

    if (.name == "names") {
        out <- purrr::map(base_url, names)
        class(out) <- c("get_url_names", class(out))
    } else {
        out <- unlist(unname(base_url))[[.name]]
    }
    out
}


#' @export
print.get_url_names <- function(x, msg = TRUE, ...) {
    x$ns_prefix <- "...includes common OBO Foundry and SPARQL prefixes; use `ns_prefix` to see the complete list."
    class(x) <- class(x)[-1]
    if (msg) print(x, ...)
    invisible(x)
}
