#' Sort Vectors by Priority
#'
#' Sort vectors, prioritizing according to levels specified. Uses
#' [base::factor()] internally with unspecified arguments disabled.
#'
#' @inheritParams base::factor
#'
#' @export
priority_sort <- function(x, levels, exclude = NA) {

    x_fctr <- factor(x, levels = levels, exclude = exclude, ordered = FALSE)
    x_sort <- sort(x_fctr)
    x_out <- as.character(x_sort)

    class(x_out) <- class(x)

    x_out
}
