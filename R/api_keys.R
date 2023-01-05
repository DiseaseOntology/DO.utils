#' Set Keys for Scopus API Access
#'
#' Sets Elsevier API key and/or institutional token as environment variables
#' (`Elsevier_API` and `Elsevier_insttoken`) for access by subsequent Scopus
#' functions.
#'
#' @param api_key Elsevier API key, as a string.
#' @param insttoken Elsevier institutional token, as a string.
#'
#' @seealso citedby_scopus
#'
#' @export
set_scopus_keys <- function(api_key, insttoken) {
    if (missing(api_key) && missing(insttoken)) {
        rlang::abort("At least one key must be provided")
    }

    out <- NULL
    if (!missing(api_key)) out <- Sys.setenv(Elsevier_API = api_key)
    if (!missing(insttoken)) {
        out <- c(out, Sys.setenv(Elsevier_insttoken = insttoken))
    }
    out
}

#' Ensure Scopus institutional token is in headers and, if not, adds it from
#' `insttoken` or `Elsevier_insttoken` environment variable.
#'
#' @noRd
use_scopus_insttoken <- function(insttoken = NULL, headers = NULL) {
    if (!is.null(headers)) {
        header_insttoken <- headers["X-ELS-Insttoken"]
        if (!is_missing(header_insttoken)) {
            if (!is.null(insttoken)) {
                rlang::abort("Scopus institutional token must be added with only one of `insttoken` or `headers` arguments.")
            }
            return(headers)
        }
    }

    if (is.null(insttoken) || is_missing(insttoken)) {
        insttoken <- Sys.getenv("Elsevier_insttoken")
    }

    if (is.null(insttoken) || is_missing(insttoken)) {
        rlang::abort(
            c(
                "Scopus institutional token not found. Use a method below to add it:",
                i = "Set it once per session with `set_scopus_keys(..., insttoken = <token>)`.",
                i = "Provide it directly to the function with the `insttoken` argument."
            )
        )
    }

    headers <- c(headers, rscopus::inst_token_header(insttoken))
    headers
}


# for building vignette ---------------------------------------------------

possible_api_keys <- c("ENTREZ_KEY", "Elsevier_API", "Elsevier_insttoken")

#' Checks that all API keys are available via keyring to execute vignette.
#'
#' @noRd
has_keyring_api_key <- function(key_name = "all", verbose = FALSE) {
    key_name <- match.arg(
        key_name,
        choices = c(possible_api_keys, "all"),
        several.ok = TRUE
    )
    if ("all" %in% key_name) key_name <- possible_api_keys

    if (!rlang::is_installed("keyring")) return(FALSE)

    keys <- purrr::map_chr(
        key_name,
        ~ tryCatch(keyring::key_get(.x), error = function(e) "")
    )

    key_missing <- keys == "" | is.na(keys) | length(keys) != length(key_name)

    if (any(key_missing) && verbose) {
        rlang::inform(
            paste0(
                "Keys missing: ",
                vctr_to_string(key_name[key_missing], ", ")
            )
        )
    }
    invisible(!any(key_missing))
}
