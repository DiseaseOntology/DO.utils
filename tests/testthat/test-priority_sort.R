
# Data --------------------------------------------------------------------

chr <- letters[1:5]
int <- 1:5
dbl <- c(1.0, 0.2, 63.1, 12.7, -11.5)

test_that("priority_sort works", {
    expect_identical(priority_sort(chr, rev(chr)), rev(chr))
    expect_identical(priority_sort(int, rev(int)), rev(int))
    expect_identical(priority_sort(dbl, rev(dbl)), rev(dbl))
})
