#' Convert (Nested) List to/from Character Vector
#'
#' These functions convert a (nested) list to or from a character vector of the
#' same length, preserving and restoring the list structure and values using
#' JSON strings. They do not perform _any_ flattening or simplification and the
#' JSON strings are verbose.
#'
#' @param .list A (nested) list.
#' @param .json_chr A character vector containing the JSON representation of
#' (nested) list elements below the top level.
#'
#' @export
confine_list <- function(.list) {
    purrr::map_chr(
        .list,
        jsonlite::serializeJSON
    )
}

#' @rdname confine_list
#' @export
release_list <- function(.json_chr) {
    purrr::map(
        .json_chr,
        jsonlite::unserializeJSON
    )
}
