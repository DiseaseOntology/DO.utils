# input & expected output
x <- c("http://purl.obolibrary.org/obo/DOID_0001816", "DOID:4",
       "obo:DOID_14566", "DOID_0040001")

curie <- c("DOID:0001816", "DOID:4", "DOID:14566", "DOID:0040001")
uri <- c("http://purl.obolibrary.org/obo/DOID_0001816",
         "http://purl.obolibrary.org/obo/DOID_4",
         "http://purl.obolibrary.org/obo/DOID_14566",
         "http://purl.obolibrary.org/obo/DOID_0040001")
obo_curie <- c("obo:DOID_0001816", "obo:DOID_4", "obo:DOID_14566",
               "obo:DOID_0040001")
basename <- c("DOID_0001816", "DOID_4", "DOID_14566", "DOID_0040001")

# tests
test_that("format_doid() works", {
    expect_identical(format_doid(x, as = "URI"), uri)
    expect_identical(format_doid(x, as = "CURIE"), curie)
    expect_identical(format_doid(x, as = "obo_CURIE"), obo_curie)
    expect_identical(format_doid(x, as = "basename"), basename)
})

test_that("format_doid() allow_bare switch works", {
    w_bare <- c(x, "0001816")
    bare_expected <- c(curie, "DOID:0001816")

    expect_error(format_doid(w_bare))
    expect_identical(format_doid(w_bare, allow_bare = TRUE), bare_expected)
})
