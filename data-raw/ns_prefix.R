## code to prepare prefix datasets

#### REQUIRES program: robot (any version; see http://robot.obolibrary.org/)
prefix_json <- system2("robot", args = "export-prefixes", stdout = TRUE)
prefix <- jsonlite::fromJSON(prefix_json)[[1]] %>%
    unlist()


# non-OBO prefixes --------------------------------------------------------

not_obo_prefix <- prefix[!stringr::str_detect(prefix, "/obo")]
not_obo_prefix <- not_obo_prefix %>%
    append(
        c(
            EFO = "http://www.ebi.ac.uk/efo/EFO_",
            ORDO = "http://www.orpha.net/ORDO/Orphanet_",
            mesh = "https://id.nlm.nih.gov/mesh/",
            meshv = "http://id.nlm.nih.gov/mesh/vocab#",
            up = "http://purl.uniprot.org/core/",
            up_keywords = "http://purl.uniprot.org/keywords/",
            up_disease = "http://purl.uniprot.org/diseases/"
        )
    ) %>%
    sort()

# switch prefixes to match use in DO
names(not_obo_prefix) <- dplyr::recode(
    names(not_obo_prefix),
    "dc" = "terms",
    "dc11" = "dc"
)


# Standard OBO prefixes ---------------------------------------------------

# ROBOT prefixes should stay up-to-date as new ontologies are added to the OBO
# Foundry, see https://github.com/ontodev/robot/issues/51
obo_general <- prefix[stringr::str_detect(prefix, "/obo(/|InOwl#)$")] %>%
    sort(decreasing = TRUE)

obo_ont_prefix <- prefix[stringr::str_detect(prefix, "/obo/.")] %>%
    sort()


# Common OBO property prefix ----------------------------------------------

obo_prop_prefix <- obo_ont_prefix %>%
    stringr::str_to_lower() %>%
    stringr::str_replace("_$", "#")
names(obo_prop_prefix) <- stringr::str_to_lower(names(obo_prop_prefix))


# All OBO prefixes --------------------------------------------------------

obo_prefix <- c(obo_ont_prefix, obo_ont_prop_prefix, obo_general)


# All prefixes ------------------------------------------------------------
# ordered specific-to-general (with standard OBO ontology prefixes first)

ns_prefix <- c(
    obo_prefix,
    not_obo_prefix
)

usethis::use_data(not_obo_prefix, overwrite = TRUE)
usethis::use_data(obo_prefix, overwrite = TRUE)
usethis::use_data(obo_ont_prefix, overwrite = TRUE)
usethis::use_data(obo_prop_prefix, overwrite = TRUE)
usethis::use_data(ns_prefix, overwrite = TRUE)
usethis::use_r("data", open = TRUE)
