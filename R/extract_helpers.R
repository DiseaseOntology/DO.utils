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
    WHERE {
        ?class a owl:Class ;
            rdfs:subClassOf* {{ top_class }} ;
            rdfs:subClassOf ?parent ;
            oboInOwl:id ?id ;
            rdfs:label ?label .
        ?parent oboInOwl:id ?parent_id ;
            rdfs:label ?parent_label .
        FILTER(!isblank(?parent))
    }'
