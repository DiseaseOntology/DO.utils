replace_www_duplicate <- function(url, how = "remove") {
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


#' Try a URL
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


#' Parse try_url HTTP response
#'
#' Parse an HTTP response from [try_url()], including any potential R errors.
#' _Currently only works for HEAD and GET requests._
#'
#' @param resp The response from `try_url()`.
#' @param include_resp Whether the full HTTP response should be included in
#'     the `tibble` as a list column, `TRUE` (default) or `FALSE`.
#' @param content Arguments used to extract `GET` content, as a named list
#'      matching [httr::content()] arguments (`as`, `type`, `encoding`, etc.).
#'
#' @returns
#' `tibble` with columns `url`, `status`, `status_code`, and either `exception`
#' if an R exception occurred and no HTTP response is available or
#' `redirect_url` if an HTTP response was received.
#'
#' Optionally includes the full HTTP `response`.
#'
#' **For `GET` responses**, additionally includes a `content` list-column.
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
                paste0("R", std_type[std_type %in% class(exc)]),
                paste0("R", exc[1])
            ),
            status_code = NA_integer_,
            exception = conditionMessage(exc)
        )

        if (type == "GET") {
            resp_tidy$content <- NA
        }

    } else {
        last_url <- purrr::map(resp$all_headers, ~ .x$headers$location) %>%
            unlist() %>%
            tail(1)
        resp_tidy <- tibble::tibble(
            url = resp$url,
            status = httr::http_status(resp)$category,
            status_code = httr::status_code(resp),
            # include redirect URL, where applicable
            redirect_url = dplyr::if_else(
                is.null(last_url) || last_url == resp$url,
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
