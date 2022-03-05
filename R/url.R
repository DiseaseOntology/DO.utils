# Ensure respectful URL testing -----------------------------------------------

#' Store URL Audit Results
#'
#' Creates a store for URL audits that persists, should [url_audit()] fail
#' due to errors.
store_url_audit <- function() {
    url <- NULL
    domain <- NULL
    progress <- NULL
}


check_robotstxt <- function(url_df, if_pages = 10,
                           .user_agent = pkg_user_agent, ...) {
    # df <- tibble::tibble(
    #     url = url,
    #     domain = robotstxt:::guess_domain(url)
    # )

    domain_n <- dplyr::count(url_df, domain, name = "pages") %>%
        dplyr::filter(pages > if_pages) %>%
        dplyr::arrange(desc(pages))

    dom_uniq <- unique(domain_df[[domain]])
    dom_robots <- purrr::map(
        dom_uniq,
        ~ robotstxt::robotstxt(.x, user_agent = .user_agent)
    )

    url_check <- url_df %>%
        dplyr::group_by(.data$domain) %>%
        dplyr::mutate(



    delay = purrr::map_int(robots, get_delay)
    domain_df
}



get_delay <- function(robotstxt, .user_agent = pkg_user_agent,
                      default = NA_integer_) {

    cd_df <- robotstxt$crawl_delay
    if (nrow(cd_df) == 0) {
        return(default)
    }

    # delay for user agent
    delay <- dplyr::filter(cd_df, .data$useragent == .user_agent)$value
    if (is_empty(delay)) {
        delay <- dplyr::filter(cd_df, .data$useragent == "*")$value
    }

    # general delay
    if (is_empty(delay)) {
        delay <- default
    }

    as.integer(delay)
}


# make_url_allowed <- function(url, robotstxt, .user_agent = pkg_user_agent, ...) {
#     .df <- tibble::tibble(
#         url = url,
#         allowed = robotstxt$check(url, bot = .user_agent, ...),
#         dup = all_duplicated(url),
#         trimmable = dplyr::if_else(allowed, NA, TRUE)
#     )
#
#     while (
#         any(isFALSE(.df$allowed) & !.df$dup & .df$trimmable, na.rm = TRUE)
#     )  {
#         .df <- .df %>%
#             dplyr::mutate(
#                 mod_url = dplyr::if_else(.data$allowed, trim_url(.data$url), NA),
#                 allowed = dplyr::if_else(
#                     allowed,
#                     allowed,
#                     robotstxt$check(.data$mod_url, bot = .user_agent, ...)
#                 ),
#                 dup = dplyr::if_else(
#                     dup,
#                     dup,
#                     all_duplicated(c(.data$mod_url
#             )
#             dplyr::filter(!allowed)
#         recheck <- .data[recheck, ] %>%
#              %>%
#
#
#
# }


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
