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
