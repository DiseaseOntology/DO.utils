# extract_doid_url() helper
has_doid_url <- function(doid_edit) {
    stringr::str_detect(doid_edit, "url:.*DOID")
}
