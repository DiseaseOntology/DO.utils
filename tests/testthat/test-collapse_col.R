cc_df <- tibble::tibble(
     x = c(1, 2, 3, 3, 4, 4, 4),
     y = c("a", "a", "b", "b", "c", "c", "e"),
     z = c("Z", "Y", "X", "X", "W", "V", "U")
)

# collapse_col_flex() -----------------------------------------------------

test_that("all collapse_col_flex() methods works", {
    uniq_tbl <- tibble::tibble(
        x = c(1, 2, 3, 4, 4),
        y = c("a", "a", "b", "c", "e"),
        z = c("Z", "Y", "X", "W|V", "U")
    )
    expect_identical(collapse_col_flex(cc_df, z, method = "unique"), uniq_tbl)

    first_tbl <- tibble::tibble(
        x = c(1, 2, 3, 4, 4),
        y = c("a", "a", "b", "c", "e"),
        z = c("Z", "Y", "X", "W", "U")
    )
    expect_identical(collapse_col_flex(cc_df, z, method = "first"), first_tbl)

    last_tbl <- tibble::tibble(
        x = c(1, 2, 3, 4, 4),
        y = c("a", "a", "b", "c", "e"),
        z = c("Z", "Y", "X", "V", "U")
    )
    expect_identical(collapse_col_flex(cc_df, z, method = "last"), last_tbl)
})

test_that("collapse_col_flex(): duplicated rows (3-4) are collapsed with any
          column specified", {
    res <- collapse_col(cc_df, y)
    expect_equal(nrow(res), nrow(cc_df) - 1)
    expect_false(any(all_duplicated(res)))
})

