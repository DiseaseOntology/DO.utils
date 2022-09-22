#' Partition vectors
#'
#' `partition` divides vectors into partitions of a specified length and returns
#' them as a list. If `x` is not completely divisible by `n` the last list item
#' will have less than `n` elements. Similar functions may be named "chunk".
#'
#' @param x A vector.
#' @param n An integer specifying instances per partition.
#'
#' @return
#' A list with `x/n` items (rounded up) each containing `n` elements from `x`.
#' When `x %% n != 0` the last list item will have < `n` elements from `x`.
#'
#' @examples
#' partition(letters[1:10], 5)
#' partition(1:10, 3)
#'
#' @name chunk
NULL

#' @name chunk
#' @export
partition <- function(x, n) {
    pos <- seq_along(x)
    groups <- (pos - 1) %/% n
    split(x, groups)
}
