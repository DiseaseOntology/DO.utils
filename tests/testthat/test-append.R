.input_df <- tibble::tibble(x = 1:10, y = letters[1:10])

test_that("append_empty_col() adds `NA` cols at names specified", {
  expect_equal(
      append_empty_col(.input_df, "z"),
      dplyr::mutate(.input_df, z = NA)
  )
})

test_that("append_empty_col() ignores cols in `df`", {
    expect_equal(
        append_empty_col(.input_df, c("y", "z")),
        dplyr::mutate(.input_df, z = NA)
    )
    expect_equal(append_empty_col(.input_df, "x"), .input_df)
})


test_that("append_empty_col() order argument works", {
    expect_error(
        append_empty_col(.input_df, c("y", "z"), order = TRUE),
        regexp = "x"
    )
    expect_equal(
        append_empty_col(.input_df, c("x", "y", "z"), order = TRUE),
        dplyr::mutate(.input_df, z = NA)
    )
    expect_equal(
        append_empty_col(.input_df, c("x", "z", "y"), order = TRUE),
        dplyr::select(dplyr::mutate(.input_df, z = NA), x, z, y)
    )
})



# append_to_url() ---------------------------------------------------------

test_that("append_to_url() works", {
    x <- "blah"
    expect_equal(
        append_to_url(x, "doi"),
        "https://www.doi.org/blah"
    )
    expect_equal(
        append_to_url(x, "github"),
        "https://github.com/blah"
    )
    expect_equal(
        append_to_url(x, "orcid"),
        "https://orcid.org/blah"
    )
    expect_equal(
        append_to_url(x, "pubmed"),
        "https://pubmed.ncbi.nlm.nih.gov/blah"
    )
    expect_equal(
        append_to_url(x, "pmc"),
        "https://www.ncbi.nlm.nih.gov/pmc/blah"
    )
    expect_equal(
        append_to_url(x, "pmc_article"),
        "https://www.ncbi.nlm.nih.gov/pmc/articles/blah"
    )
    expect_equal(
        append_to_url(x, "alliance_disease_tsv"),
        "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz/blah"
    )
    expect_equal(
        append_to_url(x, "http://fake.url.com"),
        "http://fake.url.com/blah"
    )
    expect_equal(
        append_to_url(x, "http://fake.url.com/"),
        "http://fake.url.com/blah"
    )
})

test_that("append_to_url() preserve_NA argument works", {
    x <- c("blah", NA)
    expect_equal(
        append_to_url(x, "http://fake.url.com", preserve_NA = TRUE),
        c("http://fake.url.com/blah", NA)
    )
    expect_equal(
        append_to_url(x, "http://fake.url.com/", preserve_NA = FALSE),
        c("http://fake.url.com/blah", "http://fake.url.com/NA")
    )
})
