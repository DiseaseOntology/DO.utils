#' Convert (Nested) List to/from Character Vector
#'
#' These functions convert a (nested) list to or from a character vector of the
#' same length, preserving or restoring the list structure and values with JSON
#' strings. They are primarily designed to preserve the structure of simple
#' nested lists (no internal data.frames, matrices, or arrays), as would result
#' from an API call converted to a list without any conversion
#' (e.g. flattening).
#'
#' @section NOTE:
#' These could potentially be made to handle more diverse lists by including
#' options that control how data.frames & matrices are managed, ensuring
#' to/from procedures are linked and occur in predictable ways (see
#' arguments for [jsonlite::to/fromJSON](jsonlite::fromJSON) for details).
#'
#' @param .list a (nested) list
#' @param .json_chr a character vector containing JSON representation of list
#' elements below the top level
#'
#' @export
confine_list <- function(.list) {
    purrr::map_chr(
        .list,
        RJSONIO::toJSON,
        asIs = TRUE
    )
}

#' @rdname confine_list
#' @export
release_list <- function(.json_chr) {
    purrr::map(
        .json_chr,
        RJSONIO::fromJSON,
    )
}
