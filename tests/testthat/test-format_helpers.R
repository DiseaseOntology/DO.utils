test_that("generify_obo() works", {
    obo <- c(
        # obo_CURIEs -> ns:type
        "obo:CHEBI_24431", "obo:CL_0000000", "obo:FOODON_00002239",
        "obo:GENO_0000936", "obo:NCBITaxon_10239", "obo:SO_0000704",
        "obo:SYMP_0000900", "obo:TRANS_0000024", "obo:UBERON_0001032",
        # URI -> ns:type
        "http://purl.obolibrary.org/obo/DOID_0050117",
        # obo_CURIEs -> ns ONLY
        "obo:HP_0001197", "obo:IDO_0000664", "obo:RO_0004026"
    )

    expect_equal(
        generify_obo(obo),
        c(
            # obo_CURIEs -> ns:type
            "CHEBI:chemical", "CL:cell", "FOODON:food_material",
            "GENO:inheritance_pattern", "NCBITaxon:organism", "SO:sequence",
            "SYMP:symptom", "TRANS:pathogen_transmission", "UBERON:anatomy",
            # URI -> ns:type
            "DOID:disease",
            # obo_CURIEs -> ns ONLY
            "HP", "IDO", "RO"
        )
    )
})
