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
