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


# blank -------------------------------------------------------------------

test_that("replace_blank.default() works", {
    chr <- c("a", "b ", " ", "  ", "\r\n", "\t", NA)
    num <- c(1:2, NA)
    lgl <- c(TRUE, FALSE, NA)

    # chr
    expect_identical(
        replace_blank(chr, "Y!"),
        c("a", "b ", "Y!", "Y!", "Y!", "Y!", NA)
    )
    # ignore non-character vectors
    expect_identical(replace_blank(num, "Y!"), num)
    expect_identical(replace_null(lgl, "Y!"), lgl)
})

test_that("replace_blank.list() works for un-nested lists", {
    l2 <- list(" \t", A = "a", B = "b", Z = "  ")

    # chr
    expect_identical(
        replace_blank(l2, "Y!"),
        list("Y!", A = "a", B = "b", Z = "Y!")
    )
    # NA
    expect_identical(
        replace_blank(l2),
        list(NA_character_, A = "a", B = "b", Z = NA_character_)
    )
})

test_that("replace_blank.list() works for nested lists", {
    l3 <- list(" ", A = "a", Z = "\t", L1 = list(C = 3, Z = "\n"))

    # chr
    expect_identical(
        replace_blank(l3, "Y!"),
        list("Y!", A = "a", Z = "Y!", L1 = list(C = 3, Z = "Y!"))
    )
    # NA
    expect_identical(
        replace_blank(l3),
        list(NA_character_, A = "a", Z = NA_character_,
             L1 = list(C = 3, Z = NA_character_))
    )
})
