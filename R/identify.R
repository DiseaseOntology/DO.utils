#' Identify Obsolete (INTERNAL)
#'
#' Identify obsolete terms of the Human Disease Ontology.
#'
#' @param x An object with information about obsolete terms (use [methods()] for
#'     available classes).
#' @param ... Not used; included for extensibility.
#'
#' @keywords internal
identify_obsolete <- function(x, ...) {
    UseMethod("identify_obsolete")
}

#' @export
identify_obsolete.doid_edit <- function(x, ...) {
    match_res <- stringr::str_match(x, "Class.*DOID_([0-9]+).*obsolete")[, 2]
    obs_lui <- stats::na.omit(match_res)
    obs <- paste0("DOID:", obs_lui)
    obs
}
