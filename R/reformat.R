#' Reformat DOIDs
#'
#' Convert valid DOIDs and/or bare numbers to a specified DOID format. No
#' attempt is made to confirm bare numbers or DOIDs match existing DOIDs.
#'
#' @inheritParams is_valid_doid
#' @param to The format to convert the DOIDs to, as a string. All valid formats
#'     are possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".
#' @param allow_bare Whether bare numbers should be allowed as input, `TRUE`
#'     or `FALSE` (default).
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
#' w_bare <- c(x, "0001816")
#' reformat_doid(w_bare, allow_bare = TRUE)
#'
#' @export
reformat_doid <- function(x, to = "CURIE", allow_bare = FALSE) {
    to <- match.arg(to, choices = c("URI", "CURIE", "obo_CURIE", "basename"))
    prefix <- switch(
        to,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    if (allow_bare) {
        bare_number <- stringr::str_detect(x, "^[0-9]{1,7}$")
        assertthat::assert_that(all(is_valid_doid(x) | bare_number))

        reformatted <- dplyr::if_else(
            bare_number,
            paste0(prefix, x),
            stringr::str_replace(x, "^.*DOID[:_]", prefix)
        )
    } else {
        assertthat::assert_that(all(is_valid_doid(x)))
        reformatted <- stringr::str_replace(x, "^.*DOID[:_]", prefix)
    }

    reformatted
}
