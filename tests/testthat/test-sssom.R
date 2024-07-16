# read_sssom() tests ------------------------------------------------------

# Get file paths for test data (also used for examples in docs)
sssom_complete <- system.file(
    "extdata/foodie-inc-2022-05-01.sssom.tsv",
    package = "DO.utils",
    mustWork = TRUE
)
sssom_mapping <- system.file(
    "extdata/foodie-inc-2022-05-01-mappings.sssom.tsv",
    package = "DO.utils",
    mustWork = TRUE
)
sssom_yaml <- system.file(
    "extdata/foodie-inc-2022-05-01.sssom.yaml",
    package = "DO.utils",
    mustWork = TRUE
)

# load expected test result
load("data/sssom_test.rda")

# tests
test_that("read_sssom() works with external metadata", {
    expect_identical(read_sssom(sssom_mapping, sssom_yaml), sssom_test)
})

test_that("read_sssom() works with embedded metadata", {
    expect_identical(read_sssom(sssom_complete), sssom_test)
})

test_that("unembed_sssom_metadata() signals missing metadata appropriately", {
    sssom_missing <- sssom_test
    attr(sssom_missing, "metadata") <- NA

    expect_warning(read_sssom(sssom_mapping))
    expect_error(read_sssom(sssom_mapping, missing = "error"))
    expect_silent(
        expect_equal(
            read_sssom(sssom_mapping, metadata_missing = "none"),
            sssom_missing
        )
    )
})
