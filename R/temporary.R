#' Restore Element Names
#'
#' Restores names to elements within objects after name-removing events.
#'
#' Necessary for the following scenarios:
#'
#' * After string operations with `stringr` or `stringi`, until `stringi` fixes
#'  [issue #59](https://github.com/gagolews/stringi/issues/59)
#'
#' @param x object needing names retored
#' @param names_from object to restore names from
#'
#' @export
restore_names <- function(x, names_from) {
    purrr::set_names(x, names(names_from))
}
