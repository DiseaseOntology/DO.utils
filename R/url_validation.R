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
                request_url = url,
                valid = NA,
                status_code = NA_integer_,
                final_url = NA_character_,
                error = paste(e)
            )
        }
    )
    res_df <- if ("error" %in% names(resp)) {
        resp
    } else {
        get_resp_details(resp) %>%
            dplyr::mutate(
                request_url = url,
                resp = list(resp)
            ) %>%
            # listing URLs that aren't actual redirects is confusing and adds
            #   extra time during review
            dplyr::mutate(
                final_url = dplyr::if_else(
                    final_url == request_url,
                    NA_character_,
                    final_url
                )
            ) %>%
            dplyr::select(request_url, dplyr::everything())
    }
    res_df
}


get_resp_details <- function(resp) {
    tibble::tibble(
        valid = !httr::http_error(resp),
        status_code = httr::status_code(resp),
        final_url = resp %>%
            .$all_headers %>%
            purrr::map(~ .x$headers$location) %>%
            unlist() %>%
            tail(1)
    )
}
