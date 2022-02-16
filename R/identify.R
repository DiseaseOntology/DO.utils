#' Identify Obsolete (INTERNAL)
#'
#' Identify obsolete terms of the Human Disease Ontology.
#'
#' @param x An object with information about obsolete terms (use [methods()] for
#'     available classes).
identify_obsolete <- function(x, ...) {
    UseMethod("identify_obsolete")
}

#' @export
identify_obsolete.doid_edit <- function(x, ...) {
    stringr::str_match(x, "Class.*DOID_([0-9]+).*obsolete")[, 2] %>%
        na.omit() %>%
        paste0("DOID:", .)
}
