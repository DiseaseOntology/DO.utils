
audit_url <- function(url, user_agent = DO_agent(), delay = 1, verbose = FALSE,
                      cache = memoise::cache_memory(), resp_dir = NULL) {
    rlang::check_installed(c("httr", "polite"), reason = "to use audit_url().")

    # fallback
    if(is.null(user_agent)) {
        user_agent <- paste0("polite ", getOption("HTTPUserAgent"), " bot")
    }
    req_polite <- polite::politely(
        full_request,
        user_agent = user_agent,
        delay = delay,
        verbose = verbose,
        cache = cache
    )

    if (!is.null(resp_dir)) {
        paths <- file.path(resp_dir, paste0("resp", 1:length(url)))
        resps <- purrr::map2(url, paths, ~ req_polite(.x, .y))
        save(resps, file = file.path(resp_dir, "all.Rdata"))
    } else {
        resps <- purrr::map(url, ~ req_polite(.x))
    }
    resps
}

full_request <- function(url, path = NULL) {
    httr2::request(url) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform(path = path)
}

#' Package User Agent (INTERNAL)
#'
#' The user agent for DO.utils. For more info see
#' https://en.wikipedia.org/wiki/User_agent#Use_in_HTTP.
#'
#' @param type The user agent format to use, as a string.
#'
#' @returns
#' For "name", "version" or "full", a string. For "all", a character vector of
#' all other options.
#'
#' @noRd
#' @keywords internal
DO_agent <- function(type = "full") {
    type <- match.arg(type, c("full", "name", "version", "all"))

    .nm <- "DO.utils"
    .vers <- paste0(.nm, "/", as.character(utils::packageVersion("DO.utils")))
    .full <- paste0(.vers, " (https://github.com/DiseaseOntology/DO.utils)")

    switch(
        type,
        full = .full,
        name = .nm,
        version = .vers,
        all = c(name = .nm, version = .vers, full = .full)
    )
}
