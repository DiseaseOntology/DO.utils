# unique_if_invariant() ---------------------------------------------------

test_that("unique_if_invariant() returns 1 value when unique", {
    expect_identical(unique_if_invariant(rep("a", 2)), "a")
    expect_identical(unique_if_invariant(rep(1, 2)), 1)
    expect_identical(unique_if_invariant(rep(T, 2)), T)
    expect_identical(unique_if_invariant(rep(NA, 2)), NA)
    m <- matrix(rep(1, 4), 2)
    expect_identical(unique_if_invariant(m), matrix(rep(1, 2), 1))
    expect_identical(
        unique_if_invariant(as.data.frame(m)), as.data.frame(m)[1, ]
    )
})

test_that("unique_if_invariant() returns input value when NOT unique", {
    expect_identical(unique_if_invariant(c("a", "b")), c("a", "b"))
    expect_identical(unique_if_invariant(1:2), 1:2)
    expect_identical(unique_if_invariant(c(T, F)), c(T, F))
    expect_identical(unique_if_invariant(c(T, NA)), c(T, NA))
    m <- matrix(rep(1:2, 2), 2)
    expect_identical(unique_if_invariant(m), m)
    expect_identical(
        unique_if_invariant(as.data.frame(m)), as.data.frame(m)
    )
})

test_that("unique_if_invariant() na.rm works", {
    expect_identical(unique_if_invariant(c("a", NA), na.rm = T), "a")
    expect_identical(unique_if_invariant(c(1, NA), na.rm = T), 1)
    expect_identical(unique_if_invariant(c(T, NA), na.rm = T), T)

    m <- matrix(rep(c(1, NA), each = 2), 2) # same column
    expect_warning(
        expect_identical(unique_if_invariant(m, na.rm = T), m[1, , drop = F]),
        regexp = "na\\.rm.*ignored.*dim"
    )
    expect_warning(
        expect_identical(
            unique_if_invariant(as.data.frame(m), na.rm = T),
            as.data.frame(m)[1, ]
        ),
        regexp = "na\\.rm.*ignored.*dim"
    )

    m2 <- matrix(rep(c(1, NA), 2), 2) # same row
    expect_warning(
        expect_identical(unique_if_invariant(m2, na.rm = T), m2),
        regexp = "na\\.rm.*ignored.*dim"
    )
    expect_warning(
        expect_identical(
            unique_if_invariant(as.data.frame(m2), na.rm = T),
            as.data.frame(m2)
        ),
        regexp = "na\\.rm.*ignored.*dim"
    )
})

test_that("unique_if_invariant() incl_nm works", {
    expect_identical(
        unique_if_invariant(rep(c(a = "A"), 2), incl_nm = T),
        c(a = "A")
    )
    expect_identical(
        unique_if_invariant(rep(c(a = 1), 2), incl_nm = T),
        c(a = 1)
    )
    expect_identical(
        unique_if_invariant(rep(c(a = T), 2), incl_nm = T),
        c(a = T)
    )
    expect_identical(
        unique_if_invariant(rep(c(a = NA), 2), incl_nm = T),
        c(a = NA)
    )

    m <- matrix(rep(1, 4), 2)
    colnames(m) <- c("a", "b")
    expect_warning(
        expect_identical(
            unique_if_invariant(m, incl_nm = T),
            m[1, , drop = FALSE]
        ),
        regexp = "incl_nm.*ignored.*dim"
    )
    expect_warning(
        expect_identical(
            unique_if_invariant(as.data.frame(m), incl_nm = T),
            as.data.frame(m)[1, ]
        ),
        regexp = "incl_nm.*ignored.*dim"
    )

    # only name differs
    expect_identical(
        unique_if_invariant(c(a = "A", b = "A"), incl_nm = T),
        c(a = "A", b = "A")
    )
    # name ignored (and dropped) without incl_nm
    expect_identical(unique_if_invariant(c(a = "A", b = "A")), "A")
})


# vctr_to_string() --------------------------------------------------------

test_that("vctr_to_string() works", {
    expect_identical(vctr_to_string(c("a", "b")), "a|b")
    expect_identical(vctr_to_string(c(NA, "a")), "NA|a")
    expect_identical(vctr_to_string(NA), NA_character_)
})

test_that("vctr_to_string() na.rm works", {
    expect_identical(vctr_to_string(NA, na.rm = TRUE), NA_character_)
    expect_identical(vctr_to_string(c(NA, "a"), na.rm = TRUE), "a")
})
