#' Print OMIM inventory report statistics
#'
#' Prints OMIM inventory report statistics.
#'
#' @param x An object of class `oieb`.
#' @inheritDotParams base::print.default -quote
#'
#' @export
print.oieb <- function(x, ...) {
    oie_new <- x %>%
        dplyr::mutate(
            id_type = stringr::str_remove(.data$report, "_.+"),
            stat = stringr::str_remove(.data$report, "[^_]+_"),
            report = NULL
        )
    oie_split <- split(oie_new, oie_new$id_type)
    omim <- array(
        oie_split$omim$n,
        dim = c(1, 4),
        list("OMIM:", oie_split$omim$stat)
    )
    doid <- array(
        oie_split$doid$n,
        dim = c(1, 3),
        list("DOID:", oie_split$doid$stat)
    )

    print(omim, quote = FALSE, ...)
    print(doid, quote = FALSE, ...)
}

#' Print mapping inventory report statistics
#'
#' Prints mapping inventory report statistics.
#'
#' @param x An object of class `mieb`.
#' @inheritDotParams base::print.default -quote
#'
#' @export
print.mieb <- function(x, ...) {
    mie_new <- dplyr::mutate(
        x,
        id_type = stringr::str_remove(.data$report, "_.+"),
        stat = stringr::str_remove(.data$report, "[^_]+_"),
        report = NULL
    )
    mie_split <- split(mie_new, mie_new$id_type)
    mapping <- array(
        mie_split$mapping$n,
        dim = c(1, 4),
        list("INPUT:", mie_split$mapping$stat)
    )
    doid <- array(
        mie_split$doid$n,
        dim = c(1, 3),
        list("DOID:", mie_split$doid$stat)
    )

    print(mapping, quote = FALSE, ...)
    print(doid, quote = FALSE, ...)
}