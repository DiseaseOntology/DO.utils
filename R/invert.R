#' Invert Sublists
#'
#' Inverts elements in lists within a list such that those elements (at depth 3)
#' are regrouped by position or name within each sublist (depth 2).
#'
#' @param x A list of non-empty lists.
#' @param use_sublist_names Whether to use sublist names for inversion, as
#'     `TRUE` or `FALSE` (default). See 'Details' for specifics.
#'
#' @details
#' The `use_sublist_names` alters how sublists are inverted as follows:
#'
#' * When `FALSE`, sublist elements are inverted by position within sublists.
#' Sublists must all have the same number of elements. The original sublist
#' names will be preserved (now at depth 3) but original depth 3 names (now
#' sublists) will be dropped.
#'
#' * When `TRUE`, all sublists elements (depth 3) must be named and names must
#' not be duplicated within any given sublist. Also, matching names must exist
#' across all sublists, as names are used for inversion itself, as well as, to
#' name inverted sublists (names from depth 3 are moved to depth 2) in output.
#' _All sublists must still have the same number of elements._
#'
#' @examples
#' .l <- list(
#'     list(1, "a", TRUE),
#'     list(2, "b", FALSE)
#' )
#' invert_sublists(.l)
#'
#' # sublist element ('item') names are dropped when inverting by position but
#' #    sublist names are retained
#' .l_nm <- list(
#'     subl1 = list(item1 = 1, item2 = "a", item3 = TRUE),
#'     subl2 = list(item1 = 2, item2 = "b", item3 = FALSE)
#' )
#' invert_sublists(.l_nm, use_sublist_names = FALSE)
#'
#' # all names are retained when inverting by name
#' .res1 <- invert_sublists(.l_nm, use_sublist_names = TRUE)
#' .res1
#'
#' # names are used for inverting, so order can be different
#' .l_nm2 <- list(
#'     subl1 = list(item1 = 1, item2 = "a", item3 = TRUE),
#'     subl2 = list(item2 = "b", item3 = FALSE, item1 = 2) # order changed
#' )
#'
#' ## invert by name -> results the same, despite order change
#' .res2 <- invert_sublists(.l_nm2, use_sublist_names = TRUE)
#' identical(.res1, .res2)
#'
#' ## invert by pos -> results differ
#' .res3 <- invert_sublists(.l_nm2, use_sublist_names = FALSE)
#' identical(.res1, .res3)
#'
#' @export
invert_sublists <- function(x, use_sublist_names = FALSE) {
    stopifnot(is_boolean(use_sublist_names))
    stopifnot(is.list(x))
    if (length(x) <= 1) {
        stop("Cannot invert single sublist.")
    }

    sublist_len <- unique(purrr::map_int(x, length))
    if (length(sublist_len) != 1) {
        stop("Sublists do not have the same number of elements.")
    }
    if (sublist_len == 0) {
        stop("Cannot invert empty sublists.")
    }

    if (use_sublist_names) {

        all_sublist_nm <- purrr::map(x, names)
        if (any(c("", NA) %in% unlist(all_sublist_nm))) {
            stop("All sublist elements must be named when use_sublist_names = TRUE")
        }

        dup_nm <- purrr::map_lgl(all_sublist_nm, ~ any(duplicated(.x)))
        if (any(unlist(dup_nm))) {
            stop("All elements within a sublist must have unique names when use_sublist_names = TRUE")
        }

        nm_same <- purrr::map(
            all_sublist_nm,
            ~ all_sublist_nm[[1]] %in% .x
        ) %>%
            unlist() %>%
            matrix(nrow = sublist_len) %>%
            apply(1, function(row) dplyr::n_distinct(row) == 1)
        if (any(!nm_same)) {
            stop("Matching names must exist across all sublists when use_sublist_names = TRUE)")
        }

        out <- purrr::map(
            all_sublist_nm[[1]],
            function(nm) {
                purrr::map(x, purrr::chuck, nm)
            }
        )
        names(out) <- all_sublist_nm[[1]]
    } else {
        out <- purrr::map(
            1:sublist_len,
            function(i) {
                purrr::map(x, purrr::chuck, i)
            }
        )
    }

    out
}


#' Invert Names and Values (INTERNAL)
#'
#' Currently only supports character vectors.
#'
#' @param x A named object.
#'
#' @keywords internal
invert_nm <- function(x) {
    setNames(names(x), x)
}
