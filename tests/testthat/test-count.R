.df <- tibble::tibble(
    x = 1:3,
    y = c("1|2", "1|3", "2"),
    z = c("1", "2|3", "1|3")
)

test_that("count_delim() works", {
    res_z <- tibble::tibble(y = c("1", "2", "3"), n = c(2L, 2L, 1L))
    expect_equal(
        count_delim(.df, x),
        # x output as chr may change if lengthen_col is limited to chr inputs
        tibble::tibble(x = as.character(1:3), n = rep(1, 3))
    )
    expect_equal(count_delim(.df, y), res_z)
    # accepts .data pronoun
    expect_equal(count_delim(.df, .data$y), res_z)
    expect_equal(
        count_delim(.df, y, z),
        tibble::tibble(
            y = c(rep("1", 3), rep("2", 2), rep("3", 2)),
            z = c("1", "2", "3", "1", "3", "2", "3"),
            n = c(1L, 1L, 1L, 2L, 1L, 1L, 1L)
        )
    )
})

test_that("count_delim() warns about injection", {
    expect_warning(
        expect_equal(count_delim(.df, "z"), tibble::tibble(`"z"` = "z", n = 3L))
    )
    expect_warning(
        expect_equal(
            count_delim(.df, y, "z"),
            tibble::tibble(
                y = c("1", "2", "3"),
                `"z"` = c("z", "z", "z"),
                n = c(2L, 2L, 1L)
            )
        )
    )
})
