#' Update Mappings from SSSOM
#'
#' Updates the mappings of an input using the mappings from an SSSOM file. The
#' SSSOM specification is _not_ currently supported in its entirety. Only one
#' mapping record should exist for each triple.
#'
#' @param input
#' @param exact_as_xref Whether to treat `oboInOwl:hasDbXref` as equivalent to
#' `skos:exactMatch` (default: `TRUE`).
#'
#' @family SSSOM-related functions
#' @export
sssom_update <- function(input, sssom, xref_as_exact = TRUE, replaced_out = NULL) {
    map_up <- read_sssom(sssom)
    out <- list(map_init = map_up)
    if (isTRUE(exact_as_xref)) {
        map_xref <- map_up %>%
            dplyr::filter(predicate_id == "skos:exactMatch") %>%
            dplyr::mutate(predicate_id == "oboInOwl:hasDbXref")
        map_up <- dplyr::bind_rows(map_up, map_xref)
    }

    map_exist <- extract_mappings(input)

    map_ignore <- dplyr::semi_join(
        map_up,
        map_exist,
        by = c("subject_id", "predicate_id", "predicate_modifier", "object_id")
    )
    out["map_exist"] <- map_exist
    out["map_ignore"] <- map_ignore
    out
}
