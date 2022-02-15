# extract_doid_url() helper
has_doid_url <- function(doid_edit) {
    grepl("DOID", doid_edit) & grepl("url:", doid_edit)
}
