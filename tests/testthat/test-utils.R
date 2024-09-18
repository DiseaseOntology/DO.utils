# sandwich_text() tests ---------------------------------------------------

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
    expect_error(sandwich_text("a", 1))
})

test_that("sandwich_text() add_dup argument works", {
    expect_equal(
        sandwich_text("bah", placeholder = c("b", "h"), add_dup = TRUE),
        "bbahh"
    )
    expect_equal(
        sandwich_text("bah", placeholder = c("b", "h"), add_dup = FALSE),
        "bah"
    )
    expect_equal(
        sandwich_text("bah", placeholder = "h", add_dup = FALSE),
        "hbah"
    )
})

test_that("sandwich_text() works with metacharacters", {
    expect_equal(sandwich_text("a", c("\\code{", "}")), "\\code{a}")
    expect_equal(sandwich_text("a", "\\1"), "\\1a\\1")
    # frequent use case: construct regex pattern
    expect_equal(sandwich_text("a", c("^", "$")), "^a$")
    expect_equal(sandwich_text("a", c("(?<chr>", ")")), "(?<chr>a)")
})

# length_sort() tests -----------------------------------------------------

test_that("length_sort() works", {
    x <- c("ccc", "aaaa", "eee", "b", "DDD")
    expect_equal(length_sort(x), c("b", "ccc", "eee", "DDD", "aaaa"))
    expect_equal(
        length_sort(x, decreasing = TRUE),
        c("aaaa", "ccc", "eee", "DDD", "b")
    )
})

test_that("length_sort() args inherited from order work", {
    x <- c(1:9, NA, 100, 10)
    expect_equal(length_sort(x), c(1:9, 10, 100, NA))
    expect_equal(length_sort(x, decreasing = TRUE), c(100, 10, 1:9, NA))
    expect_equal(length_sort(x, na.last = FALSE), c(NA, 1:9, 10, 100))
    expect_equal(length_sort(x, na.last = NA), c(1:9, 10, 100))
})

test_that("length_sort() by_name argument works", {
    x <- c(bb = 333, ccc = 1, a = 22)
    expect_equal(length_sort(x), c(ccc = 1, a = 22, bb = 333))
    expect_equal(
        length_sort(x, by_name = TRUE),
        c(a = 22, bb = 333, ccc = 1)
    )
    expect_equal(
        length_sort(x, by_name = TRUE, decreasing = TRUE),
        c(ccc = 1, bb = 333, a = 22)
    )
})


# length_order() tests ----------------------------------------------------

test_that("length_order() works", {
    x <- tibble::tibble(
        x = 1:3,
        y = c("b", "aa", "c"),
        z = c("bb", "a", "c")
    )

    expect_equal(length_order(x, y), x[c(1, 3, 2), ])
    expect_equal(length_order(x, c(y, z)), x[c(3, 1, 2), ])
    expect_equal(length_order(x, c(y, z), decreasing = TRUE), x[c(2, 1, 3), ])
})


# all_duplicated() tests --------------------------------------------------

na_dup <- c(NA, 1, 1:7, NA)
df_dup <- data.frame(
    x = c(NA, 1, 1:7, NA),
    y = c(NA, 1, 1:7, NA)
)

test_that("all_duplicated() works", {
    expect_equal(all_duplicated(1:10), rep(FALSE, 10))
    expect_equal(
        all_duplicated(c(1, 1:8, 1)),
        c(TRUE, TRUE, rep(FALSE, 7), TRUE)
    )
    expect_equal(
        all_duplicated(na_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
    expect_equal(
        all_duplicated(df_dup),
        c(rep(TRUE, 3), rep(FALSE, 6), TRUE)
    )
})

test_that("all_duplicated(incomparables = NA) works", {
    expect_equal(
        all_duplicated(1:10, incomparables = NA),
        rep(FALSE, 10)
    )
    expect_equal(
        all_duplicated(na_dup, incomparables = NA),
        c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    )
    # not implemented for data.frames
    # expect_equal(
    #     all_duplicated(df_dup, incomparables = NA),
    #     c(FALSE, rep(TRUE, 2), rep(FALSE, 6), FALSE)
    # )
})


# match_arg_several() tests ---------------------------------------------------

test_that("match_arg_several() works for character vectors", {
    .z <- function(x, y) { match_arg_several(x, y) }
    expect_equal(.z("a", "a"), "a")
    expect_equal(.z("a", letters[1:2]), "a")
    expect_equal(.z(letters[1:2], letters[1:2]), letters[1:2])
    expect_equal(.z(rep("a", 2), letters[1:2]), rep("a", 2))
    expect_error(
        .z("a", "b"),
        regexp = '`x` must be one of: "b"\n.*Not "a" \\(pos: 1\\)'
    )
    expect_error(
        .z(letters[1:3], letters[1:2]),
        regexp = '`x` must be one of: "a", "b"\n.*Not "c" \\(pos: 3\\)'
    )
})

test_that("match_arg_several() works for integer vectors", {
    .z <- function(x, y) { match_arg_several(x, y) }
    expect_equal(.z(1L, 1L), 1L)
    expect_equal(.z(1L, 1:2), 1L)
    expect_equal(.z(1:2, 1:2), 1:2)
    expect_equal(.z(rep(1L, 2), 1:2), rep(1L, 2))
    expect_error(
        .z(1L, 2L),
        regexp = '`x` must be one of: 2\n.*Not 1 \\(pos: 1\\)'
    )
    expect_error(
        .z(1:3, 1:2),
        regexp = '`x` must be one of: 1, 2\n.*Not 3 \\(pos: 3\\)'
    )
})


# suggest_regex() tests ---------------------------------------------------

test_that("suggest_regex() works", {
    expect_equal(suggest_regex(LETTERS), "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]")
    expect_equal(
        suggest_regex(c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD")),
        "[5ADMT][2ACHN][0ACDP][12]"
    )
    expect_equal(
        suggest_regex(
            c(paste0(LETTERS, collapse = ""), paste0(letters, collapse = ""))
        ),
        paste0("[", LETTERS, letters, "]", collapse = "")
    )
})


# roll_middle() tests -----------------------------------------------------

test_that("roll_middle() works", {
    x <- c(7, 14, 21, 25)
    expect_equal(roll_middle(x, 0), c(3.5, 10.5, 17.5, 23))
    expect_equal(roll_middle(x, 0, "min"), c(3.5, 10.5, 17.5, 23))
    expect_equal(roll_middle(x, 30, "max"), c(10.5, 17.5, 23, 27.5))
})
