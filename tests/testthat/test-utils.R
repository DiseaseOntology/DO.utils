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
