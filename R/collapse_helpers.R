# collapse_to_string() helpers --------------------------------------------

#' Convert to Character
#'
#' Provides all character conversion methods of [base::as.character()] and
#' additional methods for lists and data.frames. `to_character()` was created to
#' enable [collapse_to_string()] to handle many data types while avoiding unintended
#' conversions to character, which would have occurred if these methods were
#' added to `as.character()`.
#'
#' @inheritParams base::as.character
#'
#' @keywords internal
to_character <- function(x, ...) {
    UseMethod("to_character")
}

#' @export
to_character.list <- function(x, ...) {
    purrr::map(x, to_character) %>%
        unlist()
}

#' @export
to_character.data.frame <- to_character.list

#' @export
to_character.default <- function(x, ...) {
    base::as.character(x, ...)
}


# collapse_col_flex() helpers ---------------------------------------------

collapse_method <- function(.col, method = "unique", delim = "|", na.rm = FALSE) {

    if (method == "unique") {
        return(unique_to_string(.col, delim = delim, na.rm = na.rm))
    }

    if (na.rm) {
        .col <- stats::na.omit(.col)
    }

    method_fxn <- list(
        first = dplyr::first,
        last = dplyr::last
    )
    method_fxn[[method]](.col)
}
