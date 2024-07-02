# update_import_terms <- function(DOrepo, term_file) {
#     term_tbl <- read_term_file(term_file)
#     # ensure CURIEs
#     is_curie <- stringr::str_detect(term_tbl$id, "^[A-Za-z]+:[A-Za-z0-9]+")
#
#     if (!all(is_curie)) {
#         rlang::abort(
#             glueV("Identifiers in !<<term_file>>! must be CURIEs for successful update.")
#         )
#     }
#     prefix <- unique(stringr::str_remove(term_tbl$id, ":.*"))
#
#     imports <- read_doid_edit(file.path(DOrepo, "src/ontology/doid-edit.owl"))
#
#     r <- DOrepo(DOrepo)
#     terminal_node_query <- glueV(
#         "SELECT ?id ?label
#         WHERE {
#             ?class a owl:Class ;
#                 oboInOwl:id ?id ;
#                 rdfs:label ?label .
#             FILTER(CONTAINS(str(?class), '!<<prefix>>!_'))
#             FILTER NOT EXISTS { ?any rdfs:subClassOf ?class . }
#         }"
#     )
#     terminal_nodes <- r$doid_merged$query(terminal_node_query)
#     new_term_tbl <- dplyr::mutate(
#         term_tbl,
#         used = id %in% terminal_nodes$id
#     )
