test_that("drop_blank() works", {
    expect_identical(drop_blank(c("", "A")), "A")
    expect_identical(drop_blank(c("A", "B")), c("A", "B"))
})
