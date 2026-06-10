test_that("tidy_sparql() errors for unrecognized tidy_what values", {
  expect_error(
    tidy_sparql(data.frame(), tidy_what = "invalid"),
    regexp = "Invalid value(s) in tidy_what:",
    fixed = TRUE
  )
  expect_no_error(
    tidy_sparql(
      data.frame(),
      tidy_what = c(
        "header",
        "unnest",
        "uri_to_curie",
        "lgl_NA_FALSE",
        "as_tibble",
        "rm_lang_tag"
      )
    )
  )
})
