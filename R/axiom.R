#' Extract Class Axioms
#'
#' Extract `owl:equivalentClass` and `owl:subClassOf` axioms from the
#' doid-edit.owl file.
#'
#' @inheritParams read_doid_edit
#'
#' @returns
#' A list of two character vectors (`eq` and `subclass`) containing axioms in
#' OWL functional format.
#'
#' @export
extract_class_axiom <- function(DO_repo) {
    doid_edit <- read_doid_edit(DO_repo)
    eq_raw <- grep("EquivalentClasses", doid_edit, value = TRUE)
    subclass_raw <- grep("SubClassOf.*Object", doid_edit, value = TRUE)
    list(eq = eq_raw, subclass = subclass_raw)
}


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
