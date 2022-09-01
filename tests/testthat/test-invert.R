# invert_sublists() --------------------------------------------------------

# input
nm <- list(
    x = list(int = 1:2, let = c("a", "b")),
    y = list(int = 3:4, let = c("c", "d")),
    z = list(int = 5:6, let = c("e", "f"))
)
pos <- unname(purrr::map(nm, unname))
nm_increment <- purrr::map2(
    nm,
    1:length(nm),
    function(.x, .y) {
        purrr::set_names(.x, c(paste0(names(.x)[1], .y), names(.x)[2]))
    }
)
nm_reordered <- list(
    x = list(int = 1:2, let = c("a", "b")),
    y = list(let = c("c", "d"), int = 3:4), # reordered elements
    z = list(int = 5:6, let = c("e", "f"))
)
nested <- list(
    x = list(int = 1, let = "a", nested = list(1)),
    y = list(int = 2, nested = list(list(2)), let = "c"),
    z = list(nested = list(list(list(3))), let = "e", int = 3)
)

# expected
inv_nm <- list(
    int = list(x = 1:2, y = 3:4, z = 5:6),
    let = list(x = c("a", "b"), y = c("c", "d"), z = c("e", "f"))
)
inv_pos <- unname(purrr::map(inv_nm, unname))
inv_nm_increment <- inv_nm
names(inv_nm_increment)[1] <- ""
inv_nested_pos <- list(
    list(x = 1, y = 2, z = list(list(list(3)))),
    list(x = "a", y = list(list(2)), z = "e"),
    list(x = list(1), y = "c", z = 3)
)
inv_nested_nm <- list(
    int = list(x = 1, y = 2, z = 3),
    let = list(x = "a", y = "c", z = "e"),
    nested = list(x = list(1), y = list(list(2)), z = list(list(list(3))))
)

# tests
test_that("invert_sublists() works", {
    expect_equal(invert_sublists(pos), inv_pos)
})

test_that("invert_sublists() sublist names are ignored/dropped", {
    expect_equal(invert_sublists(nm), unname(inv_nm))
    expect_equal(invert_sublists(nm_increment), unname(inv_nm_increment))
})

test_that("invert_sublists(use_sublist_names = TRUE) works", {
    expect_equal(invert_sublists(nm, use_sublist_names = TRUE), inv_nm)
})

test_that("invert_sublists(use_sublist_names = TRUE): element order doesn't matter", {
    expect_equal(invert_sublists(nm_reordered, use_sublist_names = TRUE), inv_nm)
})

test_that("invert_sublists() errors when sublists are different lengths", {
    pos[[3]][2] <- NULL
    nm[[2]][1] <- NULL

    expect_error(invert_sublists(pos))
    expect_error(invert_sublists(nm))
    expect_error(invert_sublists(nm, use_sublist_names = TRUE))
})

test_that("invert_sublists(use_sublist_names = TRUE): names missing/differ = error", {
    expect_error(invert_sublists(pos, use_sublist_names = TRUE))
    expect_error(invert_sublists(nm_increment, use_sublist_names = TRUE))
})

test_that("invert_sublists() works with nested sublists", {
    expect_equal(invert_sublists(nested), inv_nested_pos)
    expect_equal(invert_sublists(nested, use_sublist_names = TRUE), inv_nested_nm)
})

test_that("invert_sublists() is reversible (except names lost if by pos)", {
    expect_equal(invert_sublists(invert_sublists(pos)), pos)
    expect_equal(
        invert_sublists(
            invert_sublists(nm, use_sublist_names = TRUE),
            use_sublist_names = TRUE
        ),
        nm
    )
    expect_equal(invert_sublists(invert_sublists(nm)), pos)
})
