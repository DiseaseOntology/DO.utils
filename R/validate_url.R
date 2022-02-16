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
