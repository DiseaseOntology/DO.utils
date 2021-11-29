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
#' @param key_name The name of a key, as a string (case-sensitive).
#' @inheritDotParams keyring::key_get
#'
#' @return
#' The value of the key/secret.
#'
#' @noRd
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

    inform_of_loc("DO.utils.key_msg", key_name, key_loc)
    key_val
}


#' Inform of Key/Secret Location
#'
#' Prints a message informing the user of where a key/secret was located.
#' Currently, the only possibilities are from the environment as an
#' environment variable or from the OS-specific credential store via keyring.
#'
#' @param option The name of the global option controlling all key/secret
#'     messages, as a string.
#' @param key The name of the key/secret, as a string.
#' @param loc The location of the key, as determined by [get_key()].
#'
#' @noRd
inform_of_loc <- function(option, key, loc) {
    # if pkg-wide key_msg = TRUE, always emit message (always ignore if FALSE)
    opt <- getOption(option)
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
        key_option <- paste(option, key, sep = ".")
        if (getOption(key_option, TRUE)) {
            message(key, " was obtained from ", loc_msg)

            options(
                purrr::set_names(list(FALSE), nm = key_option)
            )
        }
    }
}
