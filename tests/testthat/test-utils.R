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
