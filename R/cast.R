#' Create a String from Inputs
#'
#' Creates a single string from one or more inputs with each value separated by
#' `delim`.
#'
#' @param ... One or more R objects (vector, list, data.frame, etc).
#' @inheritParams vctr_to_string
#' @param unique Whether to include only unique values (across all inputs), as
#'     a logical.
#'
#' @examples
#' # collapses individual vectors
#' cast_to_string(1:10)
#'
#' # input order is preserved
#' cast_to_string(1:2, letters[1:2])
#' cast_to_string(data.frame(x = 1:2, y = letters[1:2]))
#'
#' # factor levels are captured (instead of numeric placeholders)
#' cast_to_string(factor(letters[1:2]), "c")
#'
#' # unique applies across all inputs, order is determined by first appearance
#' cast_to_string(c(3, 1, 2), 1:4, unique = FALSE)
#' cast_to_string(c(3, 1, 2), 1:4, unique = TRUE)
#'
#' @export
cast_to_string <- function(..., delim = "|", na.rm = FALSE, unique = FALSE) {
    assert_scalar_logical(na.rm)

    x <- to_character(list(...))
    if (unique) {
        string <- unique_to_string(x, delim = delim, na.rm = na.rm)
    } else {
        string <- vctr_to_string(x, delim = delim, na.rm = na.rm)
    }

    string
}


# cast_to_string() helpers ------------------------------------------------

#' Convert to Character
#'
#' Provides all character conversion methods of [base::as.character()] and
#' additional methods for lists and data.frames. `to_character()` was created to
#' enable [cast_to_string()] to handle many data types while avoiding unintended
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
