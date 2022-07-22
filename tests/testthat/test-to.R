test_that("to_curie() works", {
    uri <- c(
        "http://purl.obolibrary.org/obo/SYMP_0000000",
        "http://purl.obolibrary.org/obo/so#has_origin",
        "http://www.geneontology.org/formats/oboInOwl#SubsetProperty",
        "http://purl.org/dc/elements/1.1/date",
        "http://www.w3.org/2002/07/owl#deprecated",
        "http://purl.obolibrary.org/obo/UBERON_0000002",
        "http://purl.obolibrary.org/obo/doid#DO_AGR_slim",
        "http://purl.obolibrary.org/obo/DOID_0001816",
        "http://purl.org/dc/terms/license",
        "http://www.w3.org/2000/01/rdf-schema#comment",
        "not a URI",
        "http://bio2rdf.org/"
    )
    curie <- c(
        "SYMP:0000000",
        "obo:so#has_origin",
        "oboInOwl:SubsetProperty",
        "dc:date",
        "owl:deprecated",
        "UBERON:0000002",
        "doid:DO_AGR_slim",
        "DOID:0001816",
        "terms:license",
        "rdfs:comment",
        "not a URI",
        "http://bio2rdf.org/"
    )
  expect_equal(to_curie(uri), curie)
})
