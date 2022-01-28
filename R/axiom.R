extract_eq_axiom <- function(DO_repo) {
    doid_edit <- read_doid_edit(DO_repo)
    grep("EquivalentClasses", doid_edit, value = TRUE)
}

extract_subclass_axiom <- function(DO_repo) {
    doid_edit <- read_doid_edit(DO_repo)
    grep("SubClassOf.*Object", doid_edit, value = TRUE)
}
