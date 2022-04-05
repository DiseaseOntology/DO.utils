#' Try a URL (INTERNAL)
#'
#' Try to reach a URL with a request of the specified type, capturing R
#' errors/warnings if they occur.
#'
#' @param url URL to try, as a string.
#' @param type The HTTP request type, as a string.
#' @param ... Arguments passed on to the corresponding [httr](httr::httr)
#'     request function.
#'
#' @keywords internal
try_url <- function(url, type = "HEAD",
                    config = httr::user_agent(pkg_user_agent), ...) {
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

    tryCatch(
        request(url, config = config, ...),
        condition = function(c) {
            list(
                url = url,
                request = list(method = type),
                exception = c
            )
        }
    )
}
