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
                         include_raw_resp = TRUE, ...) {

    # handle URLs where server cannot be reached
    resp <- try_url(url, config = config, ...)
    res_df <- parse_try_url(resp, include_raw_resp = include_raw_resp)

    res_df
}


#' Try a URL
#'
#' Try to reach a URL with a request of the specified type, capturing R
#' errors/warnings if they occur.
#'
#' @param url URL to try, as a string.
#' @param type The HTTP request type, as a string.
#' @param ... Arguments passed on to the corresponding `httr` request function.
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
#' Parse the HTTP HEAD response obtained from [try_url()], including any
#' potential R errors.
#'
#' @param resp The HEAD response from `try_url()`.
#' @param include_raw_resp Whether the full raw response should be included in
#'     the `tibble` as a list column, `TRUE` (default) or `FALSE`.
#'
#' @returns
#' `tibble` with columns `url`, `status`, `status_code`, and either `exception`
#' if an R exception occurred and the request was not executed or `redirect_url`,
#' if the request was executed; also optionally including the full `response`.
#'
#' @keywords internal
parse_try_url <- function(resp, include_raw_resp = TRUE) {
    # handle R errors (not http errors)
    if("exception" %in% names(resp)) {

        exc <- class(resp$exception)
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
    } else {
        last_url <- resp %>%
            .$all_headers %>%
            purrr::map_chr(~ .x$headers$location) %>%
            tail(1)
        resp_tidy <- tibble::tibble(
            url = resp$url,
            status = httr::http_status(resp),
            status_code = httr::status_code(resp),
            # include redirect URL, where applicable
            redirect_url = dplyr::if_else(
                is.null(last_url) || last_url == resp$url,
                NULL,
                last_url
            )
        )

        if (isTRUE(include_raw_resp)) {
            resp_tidy$response <- resp
        }
    }
    resp_tidy
}
