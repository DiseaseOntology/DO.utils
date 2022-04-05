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


#' Parse try_url response (INTERNAL)
#'
#' Parse an HTTP response from [try_url()], including any potential R errors.
#' _Currently only works for HEAD and GET requests._
#'
#' @param resp A response from [try_url()].
#' @param include_resp Whether the full HTTP response should be included in
#'     the `tibble` as a list column, `TRUE` (default) or `FALSE`.
#' @param content Arguments used to extract `GET` content, as a named list
#'      matching [httr::content()] arguments (`as`, `type`, `encoding`, etc.).
#'
#' @returns
#' A `tibble` with columns `url`, `status`, `status_code`, `redirect_url` and,
#' if an R exception occurred, `exception` providing the exception's message.
#'
#' **For `GET` responses**, additionally includes a `content` list-column.
#'
#' Optionally, includes the full HTTP `response` (default).
#'
#' @keywords internal
parse_try_url <- function(resp, include_resp = TRUE, content = NULL) {
    type <- resp$request$method
    assertthat::assert_that(
        type %in% c("HEAD", "GET"),
        msg = paste0(
            "parse_try_url() cannot parse ",
            resp$request$method,
            " requests."
        )
    )

    # handle R errors (not http errors)
    if("exception" %in% names(resp)) {
        exc <- resp$exception
        std_type <- c("message", "warning", "error")

        resp_tidy <- tibble::tibble(
            url = resp$url,
            # set status to common R type where possible for consistency
            status = dplyr::if_else(
                any(std_type %in% class(exc)),
                paste0("R_", std_type[std_type %in% class(exc)]),
                paste0("R_", exc[1])
            ),
            status_code = NA_integer_,
            redirect_url = NA_character_,
            exception = conditionMessage(exc)
        )

        if (type == "GET") {
            resp_tidy$content <- list(NULL)
        }

    } else {
        last_url <- extract_resp_url(resp, "last")
        resp_tidy <- tibble::tibble(
            url = extract_resp_url(resp, "first"),
            status = httr::http_status(resp)$category,
            status_code = httr::status_code(resp),
            # include redirect URL, where applicable
            redirect_url = dplyr::if_else(
                is.null(last_url) || last_url == url,
                NA_character_,
                last_url
            )
        )

        if (type == "GET") {
            resp_tidy$content <- list(
                do.call(
                    httr::content,
                    args = purrr::prepend(content, list(x = resp))
                )
            )
        }

        if (isTRUE(include_resp)) {
            resp_tidy$response <- list(resp)
        }
    }
    resp_tidy
}
