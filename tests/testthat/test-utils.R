test_that("sandwich_text() works", {
    expect_equal(sandwich_text("a", "h"), "hah")
    expect_equal(sandwich_text("a", c("b", "h")), "bah")
    expect_equal(
        sandwich_text("testing a sentence", c("I'm ", ".")),
        "I'm testing a sentence."
    )
    expect_equal(
        sandwich_text(c("ally", "ail"), "s"),
        c("sallys", "sails")
    )
    expect_equal(
        sandwich_text(c("es", "a", "oo"), "t", collapse = ", "),
        "test, tat, toot"
    )
    expect_error(sandwich_text("a", 1))
})


# length_sort() tests -----------------------------------------------------

test_that("length_sort() works", {
    x <- c("ccc", "aaaa", "eee", "b", "DDD")
    expect_equal(length_sort(x), c("b", "ccc", "eee", "DDD", "aaaa"))
    expect_equal(
        length_sort(x, decreasing = TRUE),
        c("aaaa", "ccc", "eee", "DDD", "b")
    )
})

test_that("length_sort() args inherited from order work", {
    x <- c(1:9, NA, 100, 10)
    expect_equal(length_sort(x), c(1:9, 10, 100, NA))
    expect_equal(length_sort(x, decreasing = TRUE), c(100, 10, 1:9, NA))
    expect_equal(length_sort(x, na.last = FALSE), c(NA, 1:9, 10, 100))
    expect_equal(length_sort(x, na.last = NA), c(1:9, 10, 100))
})


# length_order() tests ----------------------------------------------------

test_that("length_order() works", {
    x <- tibble::tibble(
        x = 1:3,
        y = c("b", "aa", "c"),
        z = c("bb", "a", "c")
    )

    expect_equal(length_order(x, y), x[c(1, 3, 2), ])
    expect_equal(length_order(x, c(y, z)), x[c(3, 1, 2), ])
    expect_equal(length_order(x, c(y, z), decreasing = TRUE), x[c(2, 1, 3), ])
})


# all_duplicated() tests --------------------------------------------------

na_dup <- c(NA, 1, 1:7, NA)
df_dup <- data.frame(
    x = c(NA, 1, 1:7, NA),
    y = c(NA, 1, 1:7, NA)
)

test_that("all_duplicated() works", {
    expect_equal(all_duplicated(1:10), rep(FALSE, 10))
    expect_equal(
        all_duplicated(c(1, 1:8, 1)),
        c(TRUE, TRUE, rep(FALSE, 7), TRUE)
    )
    expect_equal(
        all_duplicated(na_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
    expect_equal(
        all_duplicated(df_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
})

test_that("all_duplicated(incomparables = NA) works", {
    expect_equal(
        all_duplicated(1:10, incomparables = NA),
        rep(FALSE, 10)
    )
    expect_equal(
        all_duplicated(na_dup, incomparables = NA),
        c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    )
    # not implemented for data.frames
    # expect_equal(
    #     all_duplicated(df_dup, incomparables = NA),
    #     c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    # )
})
