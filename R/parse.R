#' Parse URL (INTERNAL)
#'
#' Parses one or more URL(s) into protocol, domain, and path components.
#'
#' @section Provenance:
#' This function is originally from `robotstxt:::parse_url()` (MIT license,
#' [copyright Peter Meissner](https://github.com/ropensci/robotstxt/blob/master/LICENSE)),
#' with slight modification.
#'
#' @param url One or more URLs, as a character vector.
#'
#' @keywords internal
parse_url <- function(url) {
    match <- stringr::str_match(string = url, pattern = "(^\\w+://)?([^/]+)?(/.*)?")
    match <- match[, -1, drop = FALSE]
    df <- tibble::as_tibble(
        match,
        .name_repair = ~c("protocol", "domain", "path")
    ) %>%
        tidyr::replace_na(replace = list(path = ""))
    df
}
