# Resource Identifier Converter Tests -------------------------------------

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
    "so:has_origin",
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

test_that("to_curie() works", {
  expect_equal(to_curie(uri), curie)
})

test_that("to_uri() works", {
    expect_equal(to_uri(curie), uri)
})


# to_range() tests ---------------------------------------------------

x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)
txt <- paste0(x, "txt")

test_that("to_range() works", {
    expect_identical(to_range(x), "-2,-1,1,2,4,6-8,20-37,40,43,45")
})

test_that("to_range()'s sep argument works", {
    expect_identical(
        to_range(x, sep = c(" ", "%")),
        "-2 -1 1 2 4 6%8 20%37 40 43 45"
    )
})

test_that("to_range()'s int_fn argument works", {
    to_int <- function(x, y) as.integer(stringr::str_remove(x, y))
    # fn with arguments
    expect_identical(
        to_range(txt, to_int, y = "txt"),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
    # anonymous function
    expect_identical(
        to_range(
            txt,
            function(x) as.integer(stringr::str_remove(x, "txt"))
        ),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
    # rlang-style lambda
    expect_identical(
        to_range(txt, ~ as.integer(stringr::str_remove(.x, "txt"))),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
})

test_that("to_range() NA values are removed", {
    expect_identical(
        to_range(c(NA, x, NA)),
        "-2,-1,1,2,4,6-8,20-37,40,43,45"
    )
})

test_that("to_range()'s start_rm argument works", {
    expect_identical(
        to_range(
            txt,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            start_rm = "txt"
        ),
        "-2txt,-1txt,1txt,2txt,4txt,6-8txt,20-37txt,40txt,43txt,45txt"
    )
})

test_that("to_range()'s end_rm argument works", {
    stxt <- paste0("txt", x)
    expect_identical(
        to_range(
            stxt,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            end_rm = "txt"
        ),
        "txt-2,txt-1,txt1,txt2,txt4,txt6-8,txt20-37,txt40,txt43,txt45"
    )
})

test_that("to_range() works when a 2-long range starts at end", {
    almost_range_at_end <- c(x, 46)
    txt_version <- paste0(almost_range_at_end, "txt")

    expect_error(to_range(almost_range_at_end), regexp = NA)
    expect_error(
        to_range(
            txt_version,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            start_rm = "txt"
        ),
        regexp = NA
    )
})
