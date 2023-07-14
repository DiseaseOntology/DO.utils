#' Extract URL Domain
#'
#' VERY basic implementation.
#'
#' @family plot_def_src() helpers
#' @noRd
extract_url_domain <- function(url, drop_www = FALSE) {
    domain <- stringr::str_remove_all(url, "^(.*://)|/.*")

    if (drop_www) {
        domain <- stringr::str_remove(domain, "^www[^.]*\\.")
    }

    domain
}


#' Identifies NLM URLs of various subdomains.
#'
#' @family plot_def_src() helpers
#' @noRd
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", envir = parent.frame())
    df$Source %in% c("ncbi.nlm.nih.gov", "nlm.nih.gov") &
        stringr::str_detect(df$url, subdomain)
}


#' Standardizes publication types from PubMed and Scopus in DO_uses > citedby
#' corpus.
#'
#' @family plot_citedby() helpers
#' @noRd
clean_pub_type <- function(pub_type, as_fctr = TRUE) {
    pt_lc <- stringr::str_to_lower(pub_type)
    tidy <- dplyr::case_when(
        stringr::str_detect(pt_lc, "retract") ~ "Retracted",
        stringr::str_detect(pt_lc, "clinical trial") ~ "Clinical Trial",
        stringr::str_detect(pt_lc, "review") ~ "Review",
        stringr::str_detect(pt_lc, "conference") ~ "Conference",
        stringr::str_detect(pt_lc, "book") ~ "Book",
        stringr::str_detect(pt_lc, "journal.*article") ~ "Article",
        TRUE ~ "Other"
    )

    if (as_fctr) {
        tidy <- factor(
            tidy,
            # order by least to most important (most important at bottom of graph)
            levels = c("Retracted", "Other", "Review", "Conference", "Book",
                       "Clinical Trial", "Article")
        )
    }

    tidy
}
