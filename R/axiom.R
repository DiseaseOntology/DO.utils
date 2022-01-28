#' Extract Equivalent Class Axioms
#'
#' Extract `owl:equivalentClass` axioms from the doid-edit.owl file.
#'
#' @inheritParams read_doid_edit
#'
#' @returns
#' Equivalent class axioms in OWL functional format, as a character vector.
#'
#' @export
extract_eq_axiom <- function(DO_repo) {
    doid_edit <- read_doid_edit(DO_repo)
    grep("EquivalentClasses", doid_edit, value = TRUE)
}


#' Extract 'Subclass Of' Axioms
#'
#' Extract `owl:subClassOf` axioms from the doid-edit.owl file.
#'
#' @inheritParams read_doid_edit
#'
#' @returns
#' 'Subclass Of' axioms in OWL functional format, as a character vector.
#'
#' @export
extract_subclass_axiom <- function(DO_repo) {
    doid_edit <- read_doid_edit(DO_repo)
    grep("SubClassOf.*Object", doid_edit, value = TRUE)
}
