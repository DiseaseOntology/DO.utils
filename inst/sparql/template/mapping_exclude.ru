# Template for excluding mappings from DO
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

DELETE { ?doid ?mapping_type ?mapping }
WHERE {
    VALUES ?mapping_type {
        oboInOwl:hasDbXref
        skos:exactMatch skos:closeMatch skos:broadMatch skos:narrowMatch
        skos:relatedMatch
    }
    VALUES ?mapping { !<<mapping>>! }
    ?doid a owl:Class ;
        ?mapping_type ?mapping .
    FILTER(CONTAINS(str(?doid), "DOID"))
}
