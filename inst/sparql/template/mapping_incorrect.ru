# Template for removing incorrect mappings from DO
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX DOID: <http://purl.obolibrary.org/obo/DOID_>
PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

DELETE { ?doid ?mapping_type ?mapping }
WHERE {
    VALUES (?doid ?mapping_type ?mapping) { !<<mapping_set>>! }
    ?doid ?mapping_type ?mapping .
}
