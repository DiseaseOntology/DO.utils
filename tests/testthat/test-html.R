test_that("build_hyperlink() example works", {
    expect_equal(
        build_hyperlink(
            x = "DiseaseOntology",
            url = "github",
            as = "html",
            txt = "A hyperlink!"
        ),
        "<a href=\"https://github.com/DiseaseOntology\">A hyperlink!</a>"
    )
})

test_that("build_hyperlink() works with vectors", {
    gs_expect <- c(
        '=HYPERLINK(\"http://purl.obolibrary.org/obo/DOID_4\", \"DOID:4\")',
        '=HYPERLINK(\"https://meshb.nlm.nih.gov/record/ui?ui=D004194\", \"MESH:D004194")'
    )
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    expect_equal(
        build_hyperlink(
            x = c("4", "D004194"),
            url = c("DOID", "MESH"),
            as = "gs",
            txt = c("DOID:4", "MESH:D004194")
        ),
        gs_expect
    )

    expect_equal(
        build_hyperlink(
            x = c("4", "D004194"),
            url = c("DOID", "https://meshb.nlm.nih.gov/record/ui"),
            as = "gs",
            txt = c("DOID:4", "MESH:D004194"),
            sep = c("_", "?ui=")
        ),
        gs_expect
    )
})

test_that("build_hyperlink() works for CURIEs with known prefixes that have URLs available", {
    curie <- c("DOID:4", "ICD9CM:758.0", "ICD10CM:Q90",
               "KEGG:05034", "MESH:D004314", "NCI:C2993", "OMIM:190685",
               "ORDO:870", "SNOMEDCT_US:41040004", "UMLS_CUI:C0013080")
    expect <- c(
        '<a href="http://purl.obolibrary.org/obo/DOID_4">DOID:4</a>',
        '<a href="http://icd9cm.chrisendres.com/index.php?action=search&srchtext=758.0">ICD9CM:758.0</a>',
        '<a href="http://www.icd10data.com/Search.aspx?search=Q90">ICD10CM:Q90</a>',
        '<a href="https://www.kegg.jp/pathway/hsa05034">KEGG:05034</a>',
        '<a href="https://meshb.nlm.nih.gov/record/ui?ui=D004314">MESH:D004314</a>',
        '<a href="https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=C2993">NCI:C2993</a>',
        '<a href="http://www.omim.org/MIM:190685">OMIM:190685</a>',
        '<a href="https://www.orpha.net/consor/cgi-bin/OC_Exp.php?lng=en&Expert=870">ORDO:870</a>',
        '<a href="https://browser.ihtsdotools.org/?perspective=full&conceptId1=41040004">SNOMEDCT_US:41040004</a>',
        '<a href="https://uts.nlm.nih.gov/uts/umls/concept/C0013080">UMLS_CUI:C0013080</a>'
    )
    expect_equal(
        build_hyperlink(
            x = curie,
            url = "from_curie",
            as = "html"
        ),
        expect
    )
})

test_that("build_hyperlink() preserves CURIEs with known prefixes without URLs available", {
    curie <- "GARD:10247"
    expect_equal(
        build_hyperlink(x = curie, url = "from_curie", as = "html"),
        curie
    )
})
