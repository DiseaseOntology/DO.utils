# Data --------------------------------------------------------------------
df <- tibble::tibble(
    n = 1:2,
    interjection = list(c("oh", "wow"), "zap"),
    letter = list(c("a", "b", "c"), "d"),
    keep_list = list("please", c("don't", "unnest"))
)

df2 <- tibble::tibble(
    n = list(1:3, 4L),
    letter = list(c("a", "b"), "c"),
)

df_mix <- tibble::tibble(
    index = 1:2,
    letter = list(c("a", "b"), "c"),
    n_letter = list("A", 1:3)
)

df_nested <- tibble::tibble(
    n = list(1, 2:4),
    bool = list(TRUE, c(FALSE, TRUE)),
    tbl = tibble::tibble(
        a = c("A", "B"),
        b = c("C", "D"),
        c = list("E", c("F", "G"))
    )
)

df_list_df <- tibble::tibble(
    n = list(1, 2:4),
    bool = list(TRUE, c(FALSE, TRUE)),
    tbl = list(
        tibble::tibble(
            a = c("A", "B"),
            b = 1:2
        ),
        tibble::tibble(
            a = c("E", "F"),
            b = 3:4
        )
    ),
    tbl_type_mix = list(
        tibble::tibble(
            a = c("A", "B"),
            b = c("C", "D"),
        ),
        tibble::tibble(
            a = c("E", "F"),
            b = list("G", c("H", "I"))
        )
    )
)

# Tests -------------------------------------------------------------------

test_that("unnest_cross() can use tidyselect semantics", {
    expect_error(
        unnest_cross(df, cols = tidyselect::starts_with("inter")),
        regexp = NA
    )
})

test_that("unnest_cross() works like sequential unnest calls", {
    expect_identical(
        unnest_cross(df, cols = c(interjection, letter)),
        tidyr::unnest(df, interjection) %>%
            tidyr::unnest(letter)
    )
})

test_that("unnest_cross() always produces a cartesian product", {
    expect_equal(
        unnest_cross(df2, cols = tidyselect::everything()),
        df2 <- tibble::tibble(
            n = c(1, 1, 2, 2, 3, 3, 4),
            letter = c("a", "b", "a", "b", "a", "b", "c"),
        )
    )
})

test_that("unnest_cross() maintains strict type compatibility", {
    expect_error(
        unnest_cross(df_mix, cols = n_letter),
        class = "vctrs_error_incompatible_type"
    )
})

test_that("unnest_cross() works like unnest() with a single nested tibble", {
    # It does when list cols are of compatible size
    expect_equal(
        unnest_cross(df_nested, cols = c(bool, tbl)),
        tidyr::unnest(df_nested, cols = c(bool, tbl))
    )
    # It does NOT when list cols are not of compatible size.
    #   This is expected but is it desirable?
    #   Likely a corner case... will return to if it's ever used.
    # expect_equal(
    #     unnest_cross(df_nested, cols = tidyselect::everything()),
    #     tidyr::unnest(df_nested, cols = tidyselect::everything())
    # )
})

test_that("unnest_cross() works like sequential unnest calls for list col of dfs", {
    expect_equal(
        unnest_cross(df_list_df, cols = c(bool, tbl)),
        tidyr::unnest(df_list_df, bool) %>% tidyr::unnest(tbl)
    )
})
