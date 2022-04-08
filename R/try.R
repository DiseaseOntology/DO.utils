#' Try a URL (INTERNAL)
#'
#' Try to reach a URL with a request of the specified type, capturing R
#' errors/warnings if they occur.
#'
#' @param url URL to try, as a string.
#' @param type The HTTP request type, as a string.
#' @inheritParams httr::HEAD
#'
#' @section Note:
#' If not specified, a DO.utils-specific user agent is used for requests.
#'
#' @keywords internal
try_url <- function(url, type = "HEAD", ...) {
    type <- match.arg(
        type,
        c("HEAD", "GET", "POST", "PATCH", "PUT", "DELETE")
    )
    request <- switch(
        type,
        GET = httr::GET,
        PATCH = httr::PATCH,
        POST = httr::POST,
        HEAD = httr::HEAD,
        PUT = httr::PUT,
        DELETE = httr::DELETE
    )

    # add DO_agent (if none specified)
    params <- purrr::prepend(list(...), httr::user_agent(DO_agent("full")))
    tryCatch(
        request(url, params),
        condition = function(c) {
            list(
                url = url,
                request = list(method = type),
                exception = c
            )
        }
    )
}


#' Try robots.txt (INTERNAL)
#'
#' Try to request the robots.txt from a specified domain, capturing R
#' errors/warnings if they occur and noting HTTP status.
#'
#' @param domain The URL of a domain to try, as a string.
#'
#' @section NOTE:
#' This function performs no checks to make sure the URL is for the overall
#' domain/subdomain where robots.txt would be found and will likely return an
#' R/HTTP error with any non-domain URL.
#'
#' @examples
#' rt <- try_robots_txt("disease-ontology.org")
#' rt
#'
#' @keywords internal
try_robots_txt <- function(domain) {
    rt_url <- append_to_url(domain, "robots.txt")
    resp <- try_url(rt_url, "GET")
    rt_df <- parse_try_url(resp, content = list(as = "text"))

    rt_df
}
