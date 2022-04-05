#' Validate a URL
#'
#' Test a URL with a HEAD request to determine if it is valid.
#'
#' @param url The url to test, as a string.
#' @inheritParams httr::HEAD
#' @inheritParams parse_try_url
#'
#' @returns See [parse_try_url()] documentation.
#'
#' @export
validate_url <- function(url, config = httr::user_agent(pkg_user_agent),
                         include_resp = TRUE, ...) {

    # handle URLs where server cannot be reached
    resp <- try_url(url, config = config, ...)
    res_df <- parse_try_url(resp, include_resp = include_resp)

    res_df
}
