#' Reformat DOIDs
#'
#' Convert valid DOIDs or bare numbers to a specified DOID format. No attempt is
#' made to validate bare numbers as DOIDs.
#'
#' @inheritParams is_valid_doid
#' @param to The format to convert the DOIDs to, as a string. All valid formats
#'     are possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".
#'
#' @examples
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "DOID:4",
#'     "obo:DOID_14566",
#'     "DOID_0040001"
#' )
#'
#' reformat_doid(x, to = "URI")
#' reformat_doid(x, to = "CURIE")
#' reformat_doid(x, to = "obo_CURIE")
#' reformat_doid(x, to = "basename")
#'
#' @export
reformat_doid <- function(x, to = "CURIE") {
    assertthat::assert_that(all(is_valid_doid(x)))
    to <- match.arg(to, choices = c("URI", "CURIE", "obo_CURIE", "basename"))

    prefix <- switch(
        to,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    reformatted <- stringr::str_replace(x, "^.*DOID[:_]", prefix)

    reformatted
}
