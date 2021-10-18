#' Replace NAs with specified value.
#'
#' Replace all NAs in lists with specified value. This method of `replace_na`
#' will recurse into nested lists but will skip internal components that are
#' not lists or simple vectors themselves (e.g. data.frames, matrices, etc).
#' NOTE that coercion will occur where vectors in the list are of different
#' types than `replace`, with either the vector or `replace` being coerced
#' according to type order:
#' logical < integer < numeric < complex < character < list.
#'
#' @inheritParams tidyr::replace_na
#' @export
replace_na.list <- function(data, replace, ...) {

    assertthat::assert_that(is_scalar_vector(replace))

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

#' Replace NULLs with specified value.
#'
#' Replace NULLs (in lists) with specified value. `replace_null` can handle
#' nested lists but will skip internal components that are not lists
#' themselves (e.g. data.frames, matrices, etc). NOTE that `replace` will also
#' be added to empty lists (i.e. `list()`) but not zero.
#'
#' @param data A list (or list column in a data frame).
#' @param replace A single value to use for replacement.
#'
#' @export
replace_null <- function(data, replace) {

    assertthat::assert_that(is_scalar_vector(replace))

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
