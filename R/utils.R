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
