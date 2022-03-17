#' Parse URLs
#'
#' Parses URLs into protocol, domain, and path components. From
#' `robotstxt:::parse_url()` (MIT license, [copyright Peter
#' Meissner](https://github.com/ropensci/robotstxt/blob/master/LICENSE)), with
#' slight modification.
#'
#' @keywords internal
parse_url <- function (url) {
    match <- stringr::str_match(string = url, pattern = "(^\\w+://)?([^/]+)?(/.*)?")
    match <- match[, -1, drop = FALSE]
    df <- tibble::as_tibble(
        match,
        .name_repair = ~c("protocol", "domain", "path")
    ) %>%
        tidyr::replace_na(replace = list(path = ""))
    df
}
