x <- c("http://purl.obolibrary.org/obo/DOID_0001816", "DOID:4",
       "obo:DOID_14566", "DOID_0040001")

test_that("reformat_doid() works", {
    uri <- c("http://purl.obolibrary.org/obo/DOID_0001816",
             "http://purl.obolibrary.org/obo/DOID_4",
             "http://purl.obolibrary.org/obo/DOID_14566",
             "http://purl.obolibrary.org/obo/DOID_0040001")
    curie <- c("DOID:0001816", "DOID:4", "DOID:14566", "DOID:0040001")
    obo_curie <- c("obo:DOID_0001816", "obo:DOID_4", "obo:DOID_14566",
                   "obo:DOID_0040001")
    basename <- c("DOID_0001816", "DOID_4", "DOID_14566", "DOID_0040001")

    expect_identical(reformat_doid(x, to = "URI"), uri)
    expect_identical(reformat_doid(x, to = "CURIE"), curie)
    expect_identical(reformat_doid(x, to = "obo_CURIE"), obo_curie)
    expect_identical(reformat_doid(x, to = "basename"), basename)
})

test_that("reformat_doid() fails if any invalid DOIDs", {
    x[1] <- "0001816"
    expect_error(reformat_doid(x, to = "URI"))
})
