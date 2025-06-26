#' Omit Elements in a Character Vector
#'
#' Omits/removes all elements of specified type(s) from a character vector.
#'
#' @param x A character vector.
#' @param what The type(s) of elements to omit, as a character vector; currently
#'     only supports "blank" and "NA" elements.
#' @param identify Whether to identify the position of elements omitted,
#'     returned as values in an attribute similar to [stats::na.omit()].
#
#' @export
omit_chr <- function(x, what = c("blank", "NA"), identify = TRUE) {
    what <- match.arg(what, choices = c("blank", "NA"), several.ok = TRUE)
    fxn_list <- list(blank = is_blank, "NA" = is.na)

    omit <- purrr::map(fxn_list[what], ~.x(x))
    omit <- Reduce(`|`, omit)

    if (sum(omit, na.rm = TRUE) == 0) {
        return(x)
    }

    kept <- x[!omit]
    if (length(kept) == 0) {
        kept <- NA_character_
    }

    if (length(x) > 1 && identify) {
        attr(kept, "omit") <- which(omit)
    }
    kept
}


# omit_blank <- function(x) {
#     omit <- is_blank(x) & !is.na(x)
#     kept <- x[!omit]
#
#     # retain previous omit attributes, unless same type = overwrite
#     omit_attr <- attr(x, "omit")
#     if (!is.null(omit_attr)) {
#         omit_attr$blank <- omit
#     } else {
#         omit_attr <- list(blank = omit)
#     }
#     attr(kept, "omit") <- omit_attr
#
#     kept
# }
#
# omit_na <- function(x) {
#     omit <- is.na(x)
#     kept <- x[!omit]
#
#     # retain previous omit attributes, unless same type = overwrite
#     omit_attr <- attr(x, "omit")
#     if (!is.null(omit_attr)) {
#         omit_attr$"NA" <- omit
#     } else {
#         omit_attr <- list("NA" = omit)
#     }
#     attr(kept, "omit") <- omit_attr
#
#     kept
# }
