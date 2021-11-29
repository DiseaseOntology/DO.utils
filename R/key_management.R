#' Get a Key/Secret
#'
#' If available, retrieves a key/secret from [keyring](keyring::keyring)
#' (preferred) or an environment variable. When not available requests the
#' key's value from the user and stores it for future use (using `keyring`).
#'
#' Once per session this function will specify where the key was found, unless
#' one of two options is set:
#' 1. The global (and overriding) "DO.utils.key_msg" option will mute all
#' messages when `FALSE` or _always_ produce messages when `TRUE`.
#' 2. A key specific option, defined as "DO.utils.key_msg.{key_name}", is set
#' to `FALSE`.
#'
#' @param key_name The name of a key, as a string (case-sensitive). See
#'     `Common Keys` for a description tasks performed by DO.utils and the
#'     the keys they require/use.
#' @inheritDotParams keyring::key_get
#'
#' @section Common Keys:
#' Currently only "cited by" access (via `citedby_*()`) and related article
#' summary searches (via `search_*()`) require keys based on the service they
#' use:
#' - PubMed/PMC require a key named "ENTREZ_KEY" (see NCBI's
#'     [Entrez API documentation](https://www.ncbi.nlm.nih.gov/books/NBK25497/)).
#' - Scopus requires two keys named "Elsevier_API" and "Elsevier_insttoken".
#'     This second key is a special "institutional token" provided by
#'     Elsevier/Scopus that allows API access for non-standard use
#'     cases, which includes use of `citedby_scopus()`.
#'
#' @return
#' The value of the key/secret, invisibly.
get_key <- function(key_name, ...) {
    key_loc <- NULL
    # check for key from keyring, then environment
    from_keyring <- try(keyring::key_get(key_name), silent = TRUE)
    if (class(from_keyring) != "try-error") {
        key_loc <- "keyring"
        key_val <- from_keyring
    }

    from_renv <- Sys.getenv(key_name)
    if (!is_blank(from_renv)) {
        key_loc <- "env_var"
        key_val <- from_renv
    }

    if (is.null(key_loc)) {
        key_val <- askpass::askpass(
            prompt = paste0(key_name, " is unset. Please provide its value.")
        )
        keyring::key_set_with_value(key_name, password = key_val)
        return(key_val)
    }

    inform_of_loc(key_name, key_loc)
    invisible(key_val)
}


#' Inform of Key/Secret Location
#'
#' Prints a message informing the user of where a key/secret was located.
#' Currently, the only possibilities are from the environment as an
#' environment variable or from the OS-specific credential store via keyring.
#'
#' @param key The name of the key/secret, as a string.
#' @param loc The location of the key, as determined by [get_key()].
#'
#' @noRd
inform_of_loc <- function(key, loc) {
    # if pkg-wide key_msg = TRUE, always emit message (always ignore if FALSE)
    pkg_msg_opt <- "DO.utils.key_msg"
    opt <- getOption(pkg_msg_opt)
    loc_msg <- switch(
        loc,
        env_var = "an environment variable.",
        keyring = "your OS credential store, via keyring."
    )

    if (isTRUE(opt)) {
        message(key, " was obtained from ", loc_msg)
    }

    # if pkg-wide key_msg is unset, message once
    if (is.null(opt)) {
        key_option <- paste(pkg_msg_opt, key, sep = ".")
        if (getOption(key_option, TRUE)) {
            message(key, " was obtained from ", loc_msg)

            options(
                purrr::set_names(list(FALSE), nm = key_option)
            )
        }
    }
}
