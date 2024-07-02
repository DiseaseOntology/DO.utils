library(tidyverse)

de <- "~/Documents/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl"
exclude_raw <- readr::read_tsv("data-raw/DOID_SSSOM_exclude.tsv")
include_raw <- readr::read_tsv("data-raw/DOID_SSSOM_include.tsv")

# Eliminate all EXCLUDED terms --------------------------------------------
#### I tested and confirmed that this works - 2024-06-27 ####


# Identify mappings to exclude entirely
exclude <- exclude_raw %>%
    dplyr::filter(
        predicate_id %in% c("semapv:ObjectTermIsOutOfScopeForSubjectSource", "doid:exclude")
    ) %>%
    .$object_id

# Build SPARQL query for their removal
rm_query <- tempfile(fileext = ".ru")
readr::read_file(
    system.file(
        "sparql/template/mapping_exclude.ru",
        package = "DO.utils",
        mustWork = TRUE
    )
) %>%
    glueV(
        mapping = vctr_to_string(
            sandwich_text(exclude, placeholder = '"'),
            delim = " "
        )
    ) %>%
    readr::write_file(rm_query)

# Execute removal
# second robot() call is needed because ROBOT cannot write over input
robot(
    "query",
    input = de,
    update = rm_query,
    output = "DEL.owl"
)
robot(
    "convert",
    i = "DEL.owl",
    o = de,
    format = "ofn"
)
file.remove("DEL.owl")



# Remove incorrect mappings -----------------------------------------------
#### I tested and confirmed that this works - 2024-06-27 ####
# Note: SNOMEDCT_US dates need to match exactly. Maybe we should start annotating
#   the SNOMED version instead of including it in the prefix?

not_map  <- exclude_raw %>%
    dplyr::filter(predicate_modifier == "Not")
incorrect <- not_map %>%
    dplyr::mutate(
        predicate_id = dplyr::if_else(
            stringr::str_detect(predicate_id, "oboInOwl:hasDbXref|skos:exactMatch"),
            "oboInOwl:hasDbXref|skos:exactMatch",
            predicate_id
        )
    ) %>%
    lengthen_col(predicate_id) %>%
    unique() %>%
    dplyr::mutate(object_id = sandwich_text(object_id, '"')) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        mapping_set = sandwich_text(
            collapse_to_string(subject_id, predicate_id, object_id, delim = " "),
            placeholder = c("(", ")")
        )
    ) %>%
    .$mapping_set

# Build SPARQL query for their removal
rm_query <- tempfile(fileext = ".ru")
readr::read_file(
    system.file(
        "sparql/template/mapping_incorrect.ru",
        package = "DO.utils",
        mustWork = TRUE
    )
) %>%
    glueV(mapping_set = vctr_to_string(incorrect, delim = " ")) %>%
    readr::write_file(rm_query)

# Execute removal
# second robot() call is needed because ROBOT cannot write over input
robot(
    "query",
    input = de,
    update = rm_query,
    output = "DEL.owl"
)
robot(
    "convert",
    i = "DEL.owl",
    o = de,
    format = "ofn"
)
file.remove("DEL.owl")


# Remove incorrect mappings -----------------------------------------------
#### I tested and confirmed that this works - 2024-06-27 ####
# Note: SNOMEDCT_US dates need to match exactly. Maybe we should start annotating
#   the SNOMED version instead of including it in the prefix?

not_map  <- exclude_raw %>%
    dplyr::filter(predicate_modifier == "Not")
incorrect <- not_map %>%
    dplyr::mutate(
        predicate_id = dplyr::if_else(
            stringr::str_detect(predicate_id, "oboInOwl:hasDbXref|skos:exactMatch"),
            "oboInOwl:hasDbXref|skos:exactMatch",
            predicate_id
        )
    ) %>%
    lengthen_col(predicate_id) %>%
    unique() %>%
    dplyr::mutate(object_id = sandwich_text(object_id, '"')) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        mapping_set = sandwich_text(
            collapse_to_string(subject_id, predicate_id, object_id, delim = " "),
            placeholder = c("(", ")")
        )
    ) %>%
    .$mapping_set

# Build SPARQL query for their removal
rm_query <- tempfile(fileext = ".ru")
readr::read_file(
    system.file(
        "sparql/template/mapping_incorrect.ru",
        package = "DO.utils",
        mustWork = TRUE
    )
) %>%
    glueV(mapping_set = vctr_to_string(incorrect, delim = " ")) %>%
    readr::write_file(rm_query)

# Execute removal
# second robot() call is needed because ROBOT cannot write over input
robot(
    "query",
    input = ,
    update = rm_query,
    output = "DEL.owl"
)
robot(
    "convert",
    i = "DEL.owl",
    o = de,
    format = "ofn"
)
file.remove("DEL.owl")


# Add curated, correct mappings -------------------------------------------

# function to add robot template commands from SSSOM (include only)
set_sssom_rt <- function(x) {
    .nm <- names(x)
    rt_cmd <- dplyr::case_when(
        .nm == "subject_id" ~ "ID",
        .nm %in% accepted_mapping_types ~ paste0("A ", .nm, " SPLIT=|"),
        TRUE ~ NA # prevent robot from adding anything unexpected
    )
    out <- tibble::add_row(x, !!!purrr::set_names(rt_cmd, .nm), .before = 1)
    out
}

# check that SSSOM is as expected
accepted_mapping_types <- c(
    "oboInOwl:hasDbXref", "skos:exactMatch",
    "skos:broadMatch", "skos:narrowMatch"
)
dplyr::filter(include_raw, !predicate_id %in% accepted_mapping_types) # only accepted mapping types
dplyr::filter(include_raw, !is.na(predicate_modifier)) # no negated mappings
dplyr::filter(include_raw, is.na(subject_id) | is.na(predicate_id) | is.na(object_id)) # no incomplete triples


# build robot template
template <- tempfile(fileext = ".tsv")
rt <- include_raw %>%
    dplyr::select(subject_id, predicate_id, object_id) %>%
    dplyr::mutate(
        predicate_id = dplyr::if_else(
            stringr::str_detect(predicate_id, "oboInOwl:hasDbXref|skos:exactMatch"),
            "oboInOwl:hasDbXref|skos:exactMatch",
            predicate_id
        )
    ) %>%
    lengthen_col(predicate_id) %>%
    collapse_col(.cols = object_id) %>%
    tidyr::pivot_wider(
        names_from = predicate_id,
        values_from = object_id
    ) %>%
    set_sssom_rt()
readr::write_tsv(rt, template, na = "")

# Execute addition
robot("template", i = de, o = "DEL.owl", template = template, `merge-before` = "")
robot("convert", i = "DEL.owl", o = de, format = "ofn")
file.remove("DEL.owl")

