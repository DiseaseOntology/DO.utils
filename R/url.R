# Ensure respectful URL testing -----------------------------------------------

get_delay <- function(robotstxt, .user_agent = pkg_user_agent,
                      default = NA_integer_) {

    cd_df <- robotstxt$crawl_delay
    if (nrow(cd_df) == 0) {
        return(default)
    }

    # set delay for specified user agent, if available
    delay <- dplyr::filter(cd_df, .data$useragent == .user_agent)$value

    # set delay for unspecified user agents, if available
    if (rlang::is_empty(delay)) {
        delay <- dplyr::filter(cd_df, .data$useragent == "*")$value
    }

    # use default delay when no delay is specified
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
#' No URL validation is performed, but `append_to_url` will ensure that `url`
#' and `x` are concatenated with a single `/`.
#'
#' @param url A URL or the internal name of a URL used in this package, as a
#'     string. _See [get_url()] for the names of available internal URLs._
#' @param x Value to append, as a character vector.
#'
#' @export
append_to_url <- function(url, x) {
    url <- tryCatch(get_url(url), error = function(e) url)

    # avoid multiple consecutive slashes (strip trailing from url, leading from x)
    std_url <- stringr::str_remove(url, "/+$")
    std_x <- stringr::str_remove(x, "^/+")

    new_url <- paste0(std_url, "/", std_x)
    new_url
}
