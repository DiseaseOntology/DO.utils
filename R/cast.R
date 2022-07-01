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


cast_to_range <- function(x, int_fn = NULL, ..., sep = c(",", "-"),
                          na.rm = FALSE) {
    uniq <- unique(x)
    if (!is.numeric(uniq) || any(!is_whole_number(uniq))) {
        if (is.null(int_fn)) {
            rlang::abort(
                message = "`int_fn` must be specified when `x` is not limited to whole numbers.")
        }

        int_fn <- rlang::as_function(int_fn)
        int <- int_fn(uniq, ...)
        if (!is.integer(int)) {
            rlang::abort(
                message = paste0(
                    "`int_fn` should produce an `integer` not `", class(int), "`"
                )
            )
        }

        uniq_order <- uniq[order(int)]
        int <- sort(int)
        in_seq <- c(0, diff(int)) == 1
    } else {
        uniq_order <- sort(as.integer(uniq))
        in_seq <- c(0, diff(uniq_order)) == 1
    }

    out_vctr <- NULL
    for (.i in seq_along(in_seq)) {
        if (is.na(in_seq[.i + 1])) {
            out_vctr <- c(out_vctr, uniq_order[.i])
        } else if (!in_seq[.i]) {
            .sep <- if (in_seq[.i + 1] & in_seq[.i + 2]) sep[2] else sep[1]
            out_vctr <- c(out_vctr, uniq_order[.i], .sep)
        } else if (!in_seq[.i + 1]) {
            out_vctr <- c(out_vctr, uniq_order[.i], sep[1])
        }
    }

    paste0(out_vctr, collapse = "")
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
