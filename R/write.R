#' Tests for Write Access
#'
#' Acts as a minimal wrapper around [file.access(mode = 2)](base::file.access)
#' to test whether R has permission to write to files and/or directories.
#'
#' @inheritParams base::file.access
#'
#' @returns Logical vector of length equal to `names`. **NOTE** that this
#' _differs_ from [base::file.access()] which returns an integer vector.
#'
#' @keywords internal
write_access <- function(names) {
    x <- file.access(names, mode = 2)
    dplyr::case_when(
        x == 0 ~ TRUE,
        TRUE ~ FALSE
    )
}
