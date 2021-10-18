# NA ----------------------------------------------------------------------

test_that("replace_na.list() works for un-nested lists", {
    l2 <- list(NA, A = "a", B = c("b", NA), C = c(3, NA))

    # chr
    expect_identical(
        replace_na(l2, "Y!"),
        list("Y!", A = "a", B = c("b", "Y!"), C = c(3, "Y!"))
    )
    # dbl
    expect_identical(
        replace_na(l2, 1),
        list(1, A = "a", B = c("b", 1), C = c(3, 1))
    )
})

test_that("replace_na.list() works for nested lists", {
    l3 <- list(NA, A = "a", B = list("b", NA), list(C = c(3, NA)))

    # chr
    expect_identical(
        replace_na(l3, "Y!"),
        list("Y!", A = "a", B = list("b", "Y!"), list(C = c(3, "Y!")))
    )
    # dbl
    expect_identical(
        replace_na(l3, 1),
        list(1, A = "a", B = list("b", 1), list(C = c(3, 1)))
    )
})
