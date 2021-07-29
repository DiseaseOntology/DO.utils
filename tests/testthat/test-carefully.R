
# Data --------------------------------------------------------------------

x <- 1:3
x_na <- c(x, NA)

# Tests -------------------------------------------------------------------

test_that("it's careful", {
    expect_identical(match_carefully(x, rev(x)), rev(x))
    expect_identical(match_carefully(x_na, rev(x)), c(3:1, NA_integer_))
})
