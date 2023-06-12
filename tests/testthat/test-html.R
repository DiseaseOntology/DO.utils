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
