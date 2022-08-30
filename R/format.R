#' Format DOIDs
#'
#' Convert valid DOIDs and/or bare numbers to a specified DOID format. Input
#' _may_ be tested to ensure it matches a valid DOID format but no attempt is
#' made to confirm bare numbers or DOIDs match actual diseases in the ontology.
#'
#' @inheritParams is_valid_doid
#' @param as The format to convert the DOIDs to, as a string. All valid formats
#'     are possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".
#' @param convert_bare Whether bare numbers should be converted to canonical
#'     DOIDs, `TRUE` or `FALSE` (default).
#' @param validate_input Whether to ensure only valid DOIDs are included in `x`,
#'     `TRUE` (default) or `FALSE`. When `FALSE`, non-DOID input will be
#'     returned unchanged.
#'
#' @examples
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "DOID:4",
#'     "obo:DOID_14566",
#'     "DOID_0040001"
#' )
#'
#' format_doid(x, as = "URI")
#' format_doid(x, as = "CURIE")
#' format_doid(x, as = "obo_CURIE")
#' format_doid(x, as = "basename")
#'
#' # bare numbers can be converted to canonical DOIDs, if desired
#' w_bare <- c(x, "0050117")
#' format_doid(w_bare, convert_bare = TRUE)
#'
#' # non-DOIDs can be passed as input, if desired
#' mixed_input <- c(x, "random_text", "obo:SYMP_0000000", "0050117")
#' format_doid(mixed_input, validate_input = FALSE)
#' format_doid(mixed_input, convert_bare = TRUE, validate_input = FALSE)
#'
#' @export
format_doid <- function(x, as = "CURIE", convert_bare = FALSE,
                        validate_input = TRUE) {
    as <- match.arg(as, choices = c("URI", "CURIE", "obo_CURIE", "basename"))
    prefix <- switch(
        as,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    if (convert_bare) {
        bare_number <- stringr::str_detect(x, "^[0-9]{1,7}$")
        if (validate_input) {
            assertthat::assert_that(all(is_valid_doid(x) | bare_number))
        }

        formatted <- dplyr::if_else(
            bare_number,
            paste0(prefix, x),
            stringr::str_replace(x, "^.*DOID[:_]", prefix)
        )
    } else {
        if (validate_input) {
            assertthat::assert_that(all(is_valid_doid(x)))
        }

        formatted <- stringr::str_replace(x, "^.*DOID[:_]", prefix)
    }

    formatted
}


#' Format a Subtree
#'
#' Format a subtree, produced by [extract_subtree()], as a text-based tree
#' mirroring [disease-ontology.org](https://disease-ontology.org/).
#'
#' @param subtree_df A dataframe from [extract_subtree()].
#' @inheritParams extract_subtree
#'
#' @examples
#' \dontrun{
#'     do_owl <- {path_to_doid.owl_here}
#'     subtree <- extract_subtree(do_owl, "DOID:3070")
#'     st_formatted <- format_subtree(subtree, "DOID:3070")
#'     st_formatted
#' }
#'
#' @export
format_subtree <- function(subtree_df, top_node) {
    rlang::check_installed("tidygraph", reason = "to use `format_subtree()`")
    top_class <- format_doid(top_node, as = "CURIE")
    tg <- as_subtree_tidygraph(subtree_df, top_class)
    formatted <- pivot_subtree(tg, top_class)

    formatted
}
