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
