test_that("create_row_index() works", {
    expect_equal(create_row_index(6, 3), rep(1:2, times = 3))
    expect_equal(create_row_index(5, 3), c(rep(1:2, times = 2), 1))
    expect_equal(create_row_index(4, 3), c(1, 2, 1, 1))
})
