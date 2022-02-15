#' Extract URLs in DO (INTERNAL)
#'
#' Extract URLs from the doid-edit.owl file of the Human Disease Ontology.
#'
#' @param doid_edit The contents of the doid-edit.owl file, as a character
#'     vector (as provided by [read_doid_edit()]).
#' @param w_raw_match Whether to include the full line of doid-edit.owl where
#'     each URL was extracted from, as a boolean (default: `FALSE`).
#' @param quiet Whether warnings for multiple DOIDs associated with a single
#'     URL should be silenced, as a boolean (default: `FALSE`).
#' @param show_url_string Whether the complete URL string, including prefix
#'     should be returned, as a boolean (default: `FALSE`).
#'
#' @return
#' A tibble of DOIDs and their associated URLs.
extract_doid_url <- function(doid_edit, w_raw_match = FALSE, quiet = FALSE,
                             show_url_string = FALSE) {
    doid_w_url <- doid_edit[has_doid_url(doid_edit)]

    df <- tibble::tibble(
        raw_match = doid_w_url,
        doid = stringr::str_extract_all(doid_w_url, "DOID[:_][0-9]+"),
        url_str = stringr::str_extract_all(doid_w_url, 'url:[^"]+"')
    )

    # warn if multiple DOIDs associated with record (should only be 1)
    doid_mult <- purrr::map_int(df$doid, length) > 1L
    if (any(doid_mult) && !quiet) {
        rlang::warn(
            message = c(
                "Entries are associated with multiple DOIDs:",
                i = doid_w_url[doid_mult]
            ),
            class = "unexpected_value"
        )
    }

    # tidy
    df <- df %>%
        tidyr::unnest_longer(.data$doid) %>%
        tidyr::unnest_longer(.data$url_str) %>%
        dplyr::mutate(
            doid = stringr::str_replace(.data$doid, ".*DOID[_:]", "DOID:"),
            url = stringr::str_remove_all(.data$url_str, '^url:|"')
        )

    if (!isTRUE(show_url_string)) {
        df <- dplyr::select(df, -.data$url_str)
    }

    if (!isTRUE(w_raw_match)) {
        df <- dplyr::select(df, -.data$raw_match)
    }

    df
}

has_doid_url <- function(doid_edit) {
    grepl("DOID", doid_edit) & grepl("url:", doid_edit)
}


validate_url <- function(url, config = httr::user_agent(pkg_user_agent), ...) {
    resp <- tryCatch(
        httr::HEAD(url, config = config, ...),
        error = function(e) {
            tibble::tibble(
                url = url,
                valid = NA,
                status_code = NA_integer_,
                redirect_url = NA_character_,
                error = paste(e)
            )
        }
    )
    res_df <- if ("error" %in% names(resp)) {
        resp
    } else {
        get_resp_details(resp) %>%
            dplyr::mutate(
                url = url,
                resp = list(resp)
            ) %>%
            # listing URLs that aren't actual redirects is confusing and adds
            #   extra time during review
            dplyr::mutate(
                redirect_url = dplyr::if_else(
                    redirect_url == url,
                    NA_character_,
                    redirect_url
                )
            ) %>%
            dplyr::select(url, dplyr::everything())
    }
    res_df
}


get_resp_details <- function(resp) {
    tibble::tibble(
        valid = !httr::http_error(resp),
        status_code = httr::status_code(resp),
        redirect_url = resp %>%
            .$all_headers %>%
            purrr::map(~ .x$headers$location) %>%
            unlist() %>%
            tail(1)
    )
}
