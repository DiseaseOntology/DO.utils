# extract_doid_url() helper
has_doid_url <- function(doid_edit) {
    stringr::str_detect(doid_edit, "url:.*DOID")
}

# extract_subtree() helper
subtree_query_glue <- '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?id ?label ?parent_id ?parent_label
    WHERE {{
        ?class a owl:Class ;
            rdfs:subClassOf* { top_class } ;
            rdfs:subClassOf ?parent ;
            oboInOwl:id ?id ;
            rdfs:label ?label .
        ?parent oboInOwl:id ?parent_id ;
            rdfs:label ?parent_label .
        FILTER(!isblank(?parent))
    }}'


# extract_subclass() helpers -------------------------------------------------

prep_extract_query <- function(class, method) {
    opts <- c("self", "parents", "descendants", "ancestors", "common_ancestor")
    if (!method %in% opts) {
        rlang::abort("`method` must be one of: ", paste(opts, collapse = ", "))
    }
    q_dir <- system.file("sparql", package = "DO.utils")
    q_path <- switch(
        self = file.path(q_dir, "set.rq"),
        descendants = file.path(q_dir, "set_descendants.rq"),
        ancestors = file.path(q_dir, "set_ancestors.rq"),
        common_ancestor = file.path(q_dir, "set_to_common_ancestor.rq")
    )

    set <- paste0(class, collapse = " ")
    # only for common_ancestor
    set_values <- set <- paste0(class, collapse = ", ")
    query <- glueV(readr::read_file(q_path), set = set, set_values = set_values)

    query
}
