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
        append_to_url(x, "http://fake.url.com/"),
        "http://fake.url.com/blah"
    )
})

test_that("append_to_url() sep argument works", {
    expect_equal(
        append_to_url("blah", "http://fake.url.com", sep = "="),
        "http://fake.url.com=blah"
    )
    expect_equal(
        append_to_url("blah", "http://fake.url.com", sep = "/"),
        "http://fake.url.com/blah"
    )
    expect_equal(
        append_to_url("blah", "http://fake.url.com/", sep = "?q="),
        "http://fake.url.com/?q=blah"
    )
    expect_equal(
        append_to_url("blah", "http://fake.url.com/?q=", sep = "?q="),
        "http://fake.url.com/?q=blah"
    )
})

test_that("append_to_url() works with vectorized input", {
    expect_equal(
        append_to_url(
            c("blah", "uhhh"),
            c("http://fake.url.com", "https://this.is.it.com"),
            c("=", "/")
        ),
        c("http://fake.url.com=blah", "https://this.is.it.com/uhhh")
    )
})

test_that("append_to_url() with mixed-length inputs works", {
    expect_equal(
        append_to_url(
            c("blah", "uhhh"),
            "http://fake.url.com",
            sep = c("=", "/")
        ),
        c("http://fake.url.com=blah", "http://fake.url.com/uhhh")
    )
    expect_equal(
        append_to_url(
            c("blah", "uhhh"),
            c("http://fake.url.com", "https://this.is.it.com/"),
            sep = "/"
        ),
        c("http://fake.url.com/blah", "https://this.is.it.com/uhhh")
    )
})

test_that("append_to_url() can handle missing values", {
    expect_equal(
        append_to_url(
            c(NA_character_, "uhhh"),
            c("http://fake.url.com", "https://this.is.it.com/"),
            sep = "/"
        ),
        c(NA_character_, "https://this.is.it.com/uhhh")
    )
    expect_equal(
        append_to_url(
            c("blah", "uhhh"),
            c("http://fake.url.com", NA_character_),
            c("=", NA_character_)
        ),
        c("http://fake.url.com=blah", NA_character_)
    )
    expect_equal(
        append_to_url(
            c(NA_character_, "uhhh"),
            c("http://fake.url.com", NA_character_),
            c("=", NA_character_)
        ),
        c(NA_character_, NA_character_)
    )
    expect_error(
        append_to_url(
            c(NA_character_, "uhhh"),
            c("http://fake.url.com", "https://this.is.it.com/"),
            sep = c("/", NA_character_)
        )
    )
})
