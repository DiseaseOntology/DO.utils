#' \[DEPRECATED\] Replace NAs with specified value
#'
#' Replace all NAs in lists with specified value. This method of `replace_na`
#' will recurse into nested lists but will skip internal components that are
#' not lists or simple vectors themselves (e.g. data.frames, matrices, etc).
#' **NOTE \[REQUIRES `tidyr <= 1.1.4`\]:** Coercion will occur where vectors in
#' the list are of different types than `replace`, with either the vector or
#' `replace` being coercedaccording to type order:
#' logical < integer < numeric < complex < character < list.
#'
#' @section DEPRECATION NOTE:
#' The "unsafe" type conversion allowed by `replace_na.list()` will not work
#' with `tidyr v1.2.0`, which uses "safe" type conversion via
#' `vctrs::vec_cast()`. `replace_na.list()` will work with `tidyr <= 1.1.4`.
#' If `replace_na.list()` is not frequently needed, it will be removed in
#' future updates.
#'
#' @inheritParams tidyr::replace_na
#'
#' @export
replace_na.list <- function(data, replace, ...) {

    assertthat::assert_that(rlang::is_scalar_vector(replace))

    # identify vector elements (standard replace_na should work for these)
    out <- data
    element_class <- purrr::map_chr(
        out,
        function(.l) {
            # use the lowest level class
            c <- class(.l)
            c[[length(c)]]
        }
    )
    replace_if_class <- c("logical", "integer", "numeric", "complex",
                          "character")
    out <- purrr::map_if(
        out,
        element_class %in% replace_if_class,
        ~ replace_na(data = .x, replace = replace)
    )

    out <- purrr::map_if(
        out,
        element_class == "list",
        ~ replace_na(data = .x, , replace = replace)
    )

    # restore special class(es) of list (if any -- no guarantees here)
    class(out) <- class(data)
    out
}

#' Replace NULLs with specified value
#'
#' Replace NULLs (in lists) with specified value. `replace_null` will recurse
#' into nested lists but will skip internal components that are not lists
#' themselves (e.g. data.frames, matrices, etc). NOTE that `replace` will also
#' be added to empty lists (i.e. `list()`) but not other zero-length vectors.
#'
#' @param data A list (or list column in a data frame).
#' @param replace A single value to use for replacement.
#'
#' @export
replace_null <- function(data, replace) {

    assertthat::assert_that(rlang::is_scalar_vector(replace))

    # replace NULL in top-level vectors
    out <- data
    out <- purrr::map_if(out, .p = is.null, ~ replace)

    # replace NULL in empty lists
    out <- purrr::map_if(
        out,
        .p = ~class(.x) == "list" && length(.x) == 0,
        ~ list(replace)
    )

    # identify & replace NULL in nested lists
    out <- purrr::map_if(
        out,
        .p = ~ "list" %in% class(.x),
        .f = ~ replace_null(.x, replace)
    )

    # restore special class(es) of list (if any -- no guarantees here)
    class(out) <- class(data)
    out
}


#' Replace Blanks with Specified Value
#'
#' Replace blanks with specified value.
#'
#' @returns
#'
#' * If `data` is a vector, `replace_blank()` returns a vector of the same class
#' as `data` (only blanks in character vectors are modified).
#'
#' * If `data` is a list, `replace_blank()` will recurse into the list (as
#' necessary) and replace all blank values in character/list elements but will
#' skip other internal components (e.g. data.frames, matrices, etc).
#'
#' @param data A data object.
#' @param replace A string to use for replacement.
#' @param ... Additional arguments passed on to methods. Not currently used.
#'
#' @export
replace_blank <- function(data, replace = NA_character_, ...) {
    UseMethod("replace_blank")
}

#' @export
replace_blank.default <- function(data, replace = NA_character_, ...) {

    assertthat::assert_that(rlang::is_scalar_character(replace))
    if (!is.character(data)) {
        return(data)
    }
    data[is_blank(data)] <- replace
    data
}

#' @export
replace_blank.list <- function(data, replace = NA_character_, ...) {

    assertthat::assert_that(rlang::is_scalar_character(replace))

    # identify vector elements
    out <- data
    element_class <- purrr::map_chr(
        out,
        function(.l) {
            # use the lowest level class
            c <- class(.l)
            c[[length(c)]]
        }
    )

    out <- purrr::map_if(
        out,
        ~ is.character(.x) | is.list(.x),
        ~ replace_blank(data = .x, replace = replace)
    )

    # restore special class(es) of list (if any -- no guarantees here)
    class(out) <- class(data)
    out
}
