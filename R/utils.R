#' Sandwich Text Between Placeholders
#'
#' Sandwiches strings between one or two placeholders.
#'
#' @param x A string or character vector.
#' @param placeholder One or two placeholders to sandwich each element of `x`
#'     between. When two placeholders are provided, `x` will be sandwiched
#'     between them with the first on the left and second on the right.
#'     Otherwise, `x` will be sandwiched on both sides by the same placeholder.
#' @inheritDotParams base::paste0
#'
#' @examples
#' sandwich_text("a", placeholder = "h")
#' sandwich_text("a", placeholder = c("b", "h"))
#'
#' @family general utilities
#' @export
sandwich_text <- function(x, placeholder, ...) {
    if (length(placeholder) < 1 ||
        length(placeholder) > 2 ||
        !is.character(placeholder)
    ) {
        stop("`placeholder` must be a length-1 or -2 character vector.")
    }

    if (length(placeholder) == 1) {
        out <- paste0(placeholder, x, placeholder, ...)
    } else {
        out <- paste0(placeholder[1], x, placeholder[2], ...)
    }

    if (!is.null(names(x))) {
        names(out) <- names(x)
    }

    out
}


#' Sort by Character Length
#'
#' Sort a vector (`length_sort()`) or data.frame (`length_order()`) by
#' character length. Multiple elements of the the same length are secondarily
#' sorted by order of appearance.
#'
#' @param x A vector.
#' @param data A data.frame.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> The columns of `data`
#' to order by.
#' @inheritDotParams base::order decreasing na.last method
#' @examples
#' # Sorting vectors
#' x <- c("ccc", "aaaa", "eee", "b", "DDD")
#' length_sort(x)
#' length_sort(x, decreasing = TRUE)
#'
#' x2 <- c(1:9, NA, 100, 10)
#' length_sort(x2)
#' length_sort(x2, decreasing = TRUE)
#' length_sort(x2, na.last = NA)
#'
#' # Ordering data.frames
#' x <- tibble::tibble(
#'     x = 1:3,
#'     y = c("b", "aa", "c"),
#'     z = c("bb", "a", "c")
#' )
#'
#' length_order(x, "y")
#' length_order(x, "z")
#' length_order(x, c("y", "z"))
#' length_order(x, c("y", "z"), decreasing = TRUE)
#'
#' @export
length_sort <- function(x, ...) {
    x[order(stringr::str_length(x), ...)]
}

#' @rdname length_sort
#' @export
length_order <- function(data, cols, ...) {
    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    if (length(.cols) > 1) {
        index <- do.call(
            "order",
            c(
                purrr::map(
                    dplyr::select(data, dplyr::all_of(.cols)),
                    stringr::str_length
                ),
                ...
            )
        )
    } else {
        index <- order(stringr::str_length(dplyr::pull(data, .cols)), ...)
    }
    data[index, ]
}

#' Identify all duplicates
#'
#' Built on [base::duplicated()] but, unlike `base::duplicated()`,
#' identifies all duplicates _including_ the first occurrence.
#'
#' @inheritParams base::duplicated
#'
#' @export
all_duplicated <- function (x, ...)
{
    duplicated(x, ...) | duplicated(x, fromLast = TRUE, ...)
}


glueV <- function(..., .envir = parent.frame()) {
    glue::glue(..., .envir = .envir, .open = "!<<", .close = ">>!")
}


# For producing messages --------------------------------------------------

msg_dots <- function(.msg, ..., .which = NULL, .bullet = NULL) {
    dots <- rlang::exprs(...)
    arg <- msg_dots_(dots)
    if (!is.null(.bullet)) {
        arg <- purrr::set_names(arg, nm = rep(.bullet, length(arg)))
    }
    if (!is.null(.which)) {
        if (is.logical(.which) && length(.which) != length(dots)) {
            rlang::abort("Logical .which must be same length as `...`")
        }
        if (is.numeric(.which)) {
            in_rng <- dplyr::between(.which, 1, length(dots))
            if (!all(in_rng)) {
                rlang::abort(
                    c(
                        "Numeric .which must all be <= length of `...`",
                        i = paste0("Not: ", to_range(.which[!in_rng]))
                    )
                )
            }
        }
        arg <- arg[.which]
    }

    c(.msg, arg)
}

msg_dots_ <- function(x) {
    x_val <- dplyr::if_else(
        purrr::map_lgl(x, rlang::is_string),
        paste0('"', x, '"'),
        as.character(x)
    )
    dplyr::if_else(
        names(x) == "",
        x_val,
        paste(names(x), x_val, sep = " = ")
    )
}


# For creating code-formatted text in manuals programmatically ------------

vctr_to_mancode <- function(x, regex = NULL, use_names = FALSE) {
    if (use_names) x <- names(x)
    if (!is.null(regex)) x <- x[stringr::str_detect(x, regex)]
    # vctr_to_string(
        sandwich_text(x, c('\\code{', '}'))#, ", ")
}
