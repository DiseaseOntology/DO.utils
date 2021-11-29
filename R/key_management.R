#' UPDATE DOCUMENTATION!!!
#'
#' Checks whether a key/secret is available as an environment variable or from
#' [keyring](keyring::keyring) (preferred).
#'
#' @param key_name The name of a key, as a string (case-sensitive).
#' @inheritDotParams keyring::key_get
#'
#' @return
#' The location of the key/secret: "keyring", "env_var", or both as a character
#' vector; or `NULL` if a key/secret is not found in either location.
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


#' NEED TO DOCUMENT
inform_of_loc <- function(option, key, loc) {
    # if pkg-wide key_msg = TRUE, always emit message (always ignore if FALSE)
    opt <- getOption(option)
    if (isTRUE(opt)) {
        message(key, " was obtained from ", loc)
    }

    # if pkg-wide key_msg is unset, message once
    if (is.null(opt)) {
        key_option <- paste(option, key, sep = ".")
        if (getOption(key_option, TRUE)) {
            message(key, " was obtained from ", loc)

            options(
                purrr::set_names(list(FALSE), nm = key_option)
            )
        }
    }
}
