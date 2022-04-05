#' Standardize "www" in URLs
#'
#' Standardizes "www" in URLs by removing or adding it when URLs of a domain
#' exhibit mixed usage (some with, others without).
#'
#' @inheritParams parse_url
#' @param how How "www" should be handled. One of "remove" (default) or "add".
#'
#' @keywords internal
standardize_www_duplicate <- function(url, how = "remove") {
    how <- match.arg(how, c("remove", "add"))
    replace_domain <- switch(
        how,
        remove = function(x) stringr::str_remove(x, "^www\\."),
        add = function(x) {
            stringr::str_replace(x, "^(www\\.)?", "www\\.")
        }
    )
    url_df <- parse_url(url) %>%
        dplyr::mutate(
            new_dom = replace_domain(.data$domain),
            www_dup = all_duplicated(.data$new_dom)
        ) %>%
        dplyr::rowwise()

    replaced <- dplyr::mutate(
        url_df,
        tmp = cast_to_string(
            protocol,
            dplyr::if_else(.data$www_dup, new_dom, domain),
            path,
            delim = "",
            na.rm = TRUE
        )
    )$tmp

    replaced
}
