#' Format DOIDs
#'
#' Convert valid DOIDs and/or bare numbers to a specified DOID format. No
#' attempt is made to confirm bare numbers or DOIDs match existing diseases in
#' the ontology.
#'
#' @inheritParams is_valid_doid
#' @param as The format to convert the DOIDs to, as a string. All valid formats
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
#' format_doid(x, as = "URI")
#' format_doid(x, as = "CURIE")
#' format_doid(x, as = "obo_CURIE")
#' format_doid(x, as = "basename")
#'
#' w_bare <- c(x, "0001816")
#' format_doid(w_bare, allow_bare = TRUE)
#'
#' @export
format_doid <- function(x, as = "CURIE", allow_bare = FALSE) {
    as <- match.arg(as, choices = c("URI", "CURIE", "obo_CURIE", "basename"))
    prefix <- switch(
        as,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    if (allow_bare) {
        bare_number <- stringr::str_detect(x, "^[0-9]{1,7}$")
        assertthat::assert_that(all(is_valid_doid(x) | bare_number))

        formatted <- dplyr::if_else(
            bare_number,
            paste0(prefix, x),
            stringr::str_replace(x, "^.*DOID[:_]", prefix)
        )
    } else {
        assertthat::assert_that(all(is_valid_doid(x)))
        formatted <- stringr::str_replace(x, "^.*DOID[:_]", prefix)
    }

    formatted
}


as_subtree_tidygraph <- function(x, top_node, limit_to_tree = TRUE) {
    # keep all parent info in labels
    label_df <- DO.utils::collapse_col_flex(x, parent_id, parent_label)

    # exclude parents which are not subclasses of top_node (usually due to
    #   multi-parentage
    if (limit_to_tree) {
        df <- dplyr::filter(x, parent_id %in% id)
    } else {
        df <- x
    }

    # create tidygraph
    tg <- df %>%
        dplyr::select(id, parent_id) %>%
        tidygraph::as_tbl_graph() %>%
        # add labels
        tidygraph::left_join(
            label_df,
            by = c("name" = "id")
        )

    tg
}
