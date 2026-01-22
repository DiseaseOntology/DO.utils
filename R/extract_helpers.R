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
    sparql_stmts <- c(
        self = "?iri rdfs:subClassOf{0} ?set .",
        parents = "?iri ^rdfs:subClassOf{0,1} ?set .",
        descendants = "?iri rdfs:subClassOf* ?set .",
        ancestors = "?iri ^rdfs:subClassOf* ?set .",
        common_ancestor = "
        {
          # find the nearest common ancestor
          SELECT ?ancestor_iri (COUNT(?mid) AS ?distance)
          WHERE {
            ?ancestor_iri ^rdfs:subClassOf* ?mid .
            ?mid ^rdfs:subClassOf* !<<set_list>>! .
            FILTER(!isBlank(?ancestor_iri))
          }
          GROUP BY ?ancestor_iri
          ORDER BY ?distance
          LIMIT 1
        }

        # get info for classes in path from set to ancestor
        VALUES ?set { !<<set>>! }

        ?iri rdfs:subClassOf* ?ancestor_iri ;
          ^rdfs:subClassOf* ?set ."
    )
    opts <- names(sparql_stmts)
    if (!method %in% opts || length(method) != 1) {
        rlang::abort(
            c(
                paste0(
                    "`method` must be one of: ",
                    paste0("'", opts, "'", collapse = ", ")),
                x = paste0("'", method, "'", collapse = ", ")
            )
        )
    }

    set <- paste0(class, collapse = " ")
    sparql_selector <- sparql_stmts[method]
    if (method == "common_ancestor") {
        set_list <- paste0(class, collapse = ", ")
        sparql_selector <- glueV(sparql_selector, set_list = set_list)
    }

    q_path <- system.file(
        "sparql", "template-subclass-set.rq",
        package = "DO.utils",
        mustWork = TRUE
    )
    query <- glueV(
        readr::read_file(q_path),
        sparql_selector = sparql_selector,
        set = set
    )
    query
}
