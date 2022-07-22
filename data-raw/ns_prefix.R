## code to prepare `sparql_prefix` dataset, created 2022-07-22

#### REQUIRES program: robot (any version; see http://robot.obolibrary.org/)
prefix_json <- system2("robot", args = "export-prefixes", stdout = TRUE)
prefix <- jsonlite::fromJSON(prefix_json)[[1]] %>%
    unlist()

# add DO ontology internal prefixes
ns_prefix <- c(
    prefix,
    "doid" = "http://purl.obolibrary.org/obo/doid#",
    "symp" = "http://purl.obolibrary.org/obo/symp#"
)

# switch prefixes to match use in DO
names(ns_prefix) <- dplyr::recode(
    names(ns_prefix),
    "dc" = "terms",
    "dc11" = "dc"
)

# prioritize with more specific first (assumes length ~ specificity)
ns_prefix <- ns_prefix[order(-str_length(ns_prefix))]

usethis::use_data(ns_prefix, overwrite = TRUE)
usethis::use_r("data", open = TRUE)
