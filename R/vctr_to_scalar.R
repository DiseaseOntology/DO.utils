#' Return Unique Value for Invariant Vectors
#'
#' Returns the unique value from an input, if and only if, only 1 unique value
#' exists (_i.e._ the input is invariant), otherwise returns the original input.
#' Uniqueness is determined by [base::unique()] for flexibility but
#' `unique_if_invariant()` may fail for custom methods.
#'
#' @param x An R object, except arrays which are not supported.
#' @param na.rm A logical scalar indicating whether `NA` values should be
#'     removed (default: FALSE); powered by [stats::na.omit()] and may be
#'     limited by its methods.
#' @param incl_nm A logic scalar indicating whether names should also be
#'     examined (default: FALSE).
#' @param ... Arguments passed on to [base::unique()] methods.
#'
#' @seealso For _unconditional_ vector-to-string conversion methods, see the
#'     [vctr_to_string()] family of functions.
#'
#' @examples
#' unique_if_invariant(c("a", "a"))
#' unique_if_invariant(c("a", "b"))
#'
#' # `NA` can be ignored
#' unique_if_invariant(c("a", NA))
#' unique_if_invariant(c("a", NA), na.rm = TRUE)
#'
#' # names are ignored by default (and often dropped); to consider and preserve
#' # them use `incl_nm = TRUE`
#' unique_if_invariant(c(a = "A", b = "A"))
#' unique_if_invariant(c(a = "A", b = "A"), incl_nm = TRUE)
#' unique_if_invariant(c(a = "A", a = "A"), incl_nm = TRUE)
#'
#' # na.rm & incl_nm are ignored for matrices & data.frames due to undesirable
#' # results; as with base::unique(), matrix comparison preserves columns
#' m <- matrix(rep(1, 4), 2)
#' unique_if_invariant(m)
#'
#' .df <- data.frame(m, check.names = TRUE)
#' unique_if_invariant(.df)
#'
#' @export
unique_if_invariant <- function(x, na.rm = FALSE, incl_nm = FALSE, ...) {
    assert_scalar_logical(na.rm)
    assert_scalar_logical(incl_nm)

    ndim <- length(dim(x))
    if (ndim > 2) {
        rlang::abort(
            c(
                "unique_if_invariant() does not support objects with >2 dimensions.",
                x = paste0("`dim(x)` = ", ndim)
            )
        )
    }

    uniq <- x
    if (na.rm & !all(is.na(x))) {
        if (ndim == 0) {
            uniq <- stats::na.omit(uniq)
        } else {
            rlang::warn("`na.rm` is ignored when `x` has 2 dimensions.")
        }
    }
    uniq <- unique(uniq, ...)


    if (ndim == 2) {
        if (incl_nm) {
            rlang::warn("`incl_nm` is ignored when `x` has 2 dimensions.")
        }
        n_out <- nrow(uniq)
    } else {
        n_out <- length(uniq)
        if (incl_nm) {
            uniq_nm <- unique(names(x), ...)
            n_out <- max(n_out, length(uniq_nm))
        }
    }

    if (n_out == 1) {
        if (incl_nm && ndim == 0) names(uniq) <- uniq_nm
        uniq
    } else {
        x
    }
}


#' Convert Vectors to Strings
#'
#' Concatenate values in a vector into a single string.
#'     * `vctr_to_string` performs simple concatenation.
#'     * `unique_to_string` reduces the vector to unique values prior to
#'         concatenation.
#'
#' @param x A vector.
#' @param delim A delimiter to place between vector elements (default: "|").
#' @param na.rm A logical scalar indicating whether `NA` values should be
#'     removed (default: `FALSE`).
#' @param sort A logical scalar indicating whether values should be sorted
#'     (default: `FALSE`).
#' @inheritParams base::sort
#'
#' @seealso [unique_if_invariant()] for an alternative, _conditional_ `unique`
#'     vector-to-string conversion method
#'
#' @export
vctr_to_string <- function(x, delim = "|", na.rm = FALSE, sort = FALSE,
                           decreasing = FALSE, ...) {
    assert_scalar_logical(na.rm)

    if (all(is.na(x))) return(NA_character_)
    if (na.rm) {
        x <- stats::na.omit(x)
    }

    if (isTRUE(sort)) {
        x <- sort(x, decreasing = decreasing, ...)
    }
    paste0(x, collapse = delim)
}

#' @rdname vctr_to_string
#' @export
unique_to_string <- function(x, delim = "|", na.rm = FALSE, sort = FALSE,
                             decreasing = FALSE, ...) {
    vctr_to_string(
        unique(x),
        delim = delim,
        na.rm = na.rm,
        sort = sort,
        decreasing = decreasing,
        ...
    )
}
