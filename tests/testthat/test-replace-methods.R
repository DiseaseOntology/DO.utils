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


# NULL --------------------------------------------------------------------

test_that("replace_null() works for un-nested lists", {
    l2 <- list(NULL, A = "a", B = "b", Z = NULL)

    # chr
    expect_identical(
        replace_null(l2, "Y!"),
        list("Y!", A = "a", B = "b", Z = "Y!")
    )
    # dbl
    expect_identical(
        replace_null(l2, 1),
        list(1, A = "a", B = "b", Z = 1)
    )
    # NA
    expect_identical(
        replace_null(l2, NA),
        list(NA, A = "a", B = "b", Z = NA)
    )
})

test_that("replace_null() works for nested lists", {
    l3 <- list(NULL, A = "a", Z = NULL, L1 = list(C = 3, Z = NULL))

    # chr
    expect_identical(
        replace_null(l3, "Y!"),
        list("Y!", A = "a", Z = "Y!", L1 = list(C = 3, Z = "Y!"))
    )
    # dbl
    expect_identical(
        replace_null(l3, 1),
        list(1, A = "a", Z = 1, L1 = list(C = 3, Z = 1))
    )
    # NA
    expect_identical(
        replace_null(l3, NA),
        list(NA, A = "a", Z = NA, L1 = list(C = 3, Z = NA))
    )
})

test_that("replace_null() adds replace value to empty lists", {
    l3 <- list(list())

    # chr
    expect_identical(
        replace_null(l3, "Y!"),
        list(list("Y!"))
    )
    # dbl
    expect_identical(
        replace_null(l3, 1),
        list(list(1))
    )
    # NA
    expect_identical(
        replace_null(l3, NA),
        list(list(NA))
    )
})
