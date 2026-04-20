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


# max_paren_depth() tests -------------------------------------------------

x <- c("no parens", "a (1 deep)", "((a) and (b or (3 deep)))")
out <- setNames(c(0L, 1L, 3L), x)

test_that("max_paren_depth() works", {
    expect_equal(max_paren_depth(x), out)
})

test_that("max_paren_depth() unmatched_err arg works", {
    uopen <- "unmatched ( paren"
    uclose <- "unmatched ( paren ) )"
    expect_error(max_paren_depth(uopen), regexp = stringr::str_escape(uopen))
    expect_error(max_paren_depth(uclose), regexp = stringr::str_escape(uclose))

    expect_equal(max_paren_depth(uopen, FALSE), setNames(NA_integer_, uopen))
    expect_equal(max_paren_depth(uclose, FALSE), setNames(NA_integer_, uclose))

    x[2] <- "unmatched ( paren"
    out[2] <- NA_integer_
    names(out) <- x
    expect_error(max_paren_depth(x))
    expect_equal(max_paren_depth(x, unmatched_err = FALSE), out)
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
    x <- c("a1b", "a2b", "a3c")
    expect <- "a[123][bc]"
    class(expect) <- c("suggested_regex", "character")
    attr(expect, "details") <- tibble::tibble(
        position = c("regex", "n"),
        `1` = c("a", "3"),
        `2` = c("[123]", "3"),
        `3` = c("[bc]", "3")
    )
    expect_equal(suggest_regex(x), expect)

    # checking only output
    equal_output_only <- function(object, expected) {
        expect_equal(object, expected, ignore_attr = TRUE)
    }
    equal_output_only(suggest_regex(LETTERS), "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]")
    equal_output_only(
        suggest_regex(c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD")),
        "[5ADMT][2ACHN][0ACDP][12]"
    )
    equal_output_only(
        suggest_regex(
            c("GENO:0000146", "GENO:0000147", "GENO:0000930", "GENO:0000932")
        ),
        "GENO:0000[19][34][0267]"
    )
})

test_that("suggest_regex(pivot = 'wide') works (default)", {
    expected <- tibble::tibble(
        position = c("regex", "n"),
        `1` = c("[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", "26")
    )
    expect_equal(attr(suggest_regex(LETTERS), "details"), expected)

    expected <- tibble::tibble(
        position = c("regex", "n"),
        `1` = c("[5ADMT]", "4"),
        `2` = c("[2ACHN]", "4"),
        `3` = c("[0ACDP]", "4"),
        `4` = c("[12]", "2")
    )
    expect_equal(
        attr(
            suggest_regex(c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD")),
            "details"
        ),
        expected
    )

    # testthat uses "C" for LC_COLLATE, so this order may differ than what is on
    #   a local machine; see https://github.com/r-lib/testthat/issues/1181
    expected <- tibble::tibble(
        position = c("regex", "n"),
        `1` = c("[Aa]", "2"), `2` = c("[Bb]", "2"), `3` = c("[Cc]", "2"),
        `4` = c("[Dd]", "2"), `5` = c("[Ee]", "2"), `6` = c("[Ff]", "2"),
        `7` = c("[Gg]", "2"), `8` = c("[Hh]", "2"), `9` = c("[Ii]", "2"),
        `10` = c("[Jj]", "2"), `11` = c("[Kk]", "2"), `12` = c("[Ll]", "2"),
        `13` = c("[Mm]", "2"), `14` = c("[Nn]", "2"), `15` = c("[Oo]", "2"),
        `16` = c("[Pp]", "2"), `17` = c("[Qq]", "2"), `18` = c("[Rr]", "2"),
        `19` = c("[Ss]", "2"), `20` = c("[Tt]", "2"), `21` = c("[Uu]", "2"),
        `22` = c("[Vv]", "2"), `23` = c("[Ww]", "2"), `24` = c("[Xx]", "2"),
        `25` = c("[Yy]", "2"), `26` = c("[Zz]", "2")
    )
    expect_equal(
        attr(
            suggest_regex(
                c(paste0(LETTERS, collapse = ""), paste0(letters, collapse = ""))
            ),
            "details"
        ),
        expected
    )
})

test_that("suggest_regex(pivot = 'long') works", {
    expected <- tibble::tibble(
        position = 1L,
        regex = "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]",
        n = 26
    )
    expect_equal(attr(suggest_regex(LETTERS, "long"), "details"), expected)

    expected <- tibble::tibble(
        position = 1:4,
        regex = c("[5ADMT]", "[2ACHN]", "[0ACDP]", "[12]"),
        n = c(4, 4, 4, 2)
    )
    expect_equal(
        attr(
            suggest_regex(c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD"), "long"),
            "details"
        ),
        expected
    )

    # testthat uses "C" for LC_COLLATE, so this order may differ than what is on
    #   a local machine; see https://github.com/r-lib/testthat/issues/1181
    expected <- tibble::tibble(
        position = 1:26,
        regex = c(
            "[Aa]", "[Bb]", "[Cc]",   "[Dd]", "[Ee]", "[Ff]", "[Gg]", "[Hh]",
            "[Ii]", "[Jj]", "[Kk]",  "[Ll]", "[Mm]", "[Nn]", "[Oo]", "[Pp]",
            "[Qq]", "[Rr]", "[Ss]", "[Tt]", "[Uu]", "[Vv]", "[Ww]", "[Xx]",
            "[Yy]", "[Zz]"
        ),
        n = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
              2, 2, 2, 2)
    )
    expect_equal(
        attr(
            suggest_regex(
                c(paste0(LETTERS, collapse = ""), paste0(letters, collapse = "")),
                "long"
            ),
            "details"
        ),
        expected
    )
})


# glueV_cum() tests -------------------------------------------------------
test_that("glueV_cum() works", {
    expect_error(glueV_cum(NULL))
    expect_equal(glueV_cum("a"), glue::as_glue("a"))
    expect_equal(
        glueV_cum("who is at !<<loc>>!?", loc = "home"),
        glue::as_glue("who is at home?")
    )
    expect_equal(
        glueV_cum("who is at !<<loc>>!?", loc = "home !<<when>>!", when = "now"),
        glue::as_glue("who is at home now?")
    )
    expect_equal(
        glueV_cum(
            "who is at !<<loc>>!?",
            "!<<response>>!",
            loc = "home",
            response = "I am at !<<loc>>!!"
        ),
        glue::as_glue("who is at home?\nI am at home!")
    )
    expect_error(glueV_cum(c("a", "b")))
})

test_that("glueV_cum() .sep argument works", {
    expect_equal(
        glueV_cum(
            "who is at !<<loc>>!?",
            "!<<response>>!",
            loc = "home",
            response = "I am at !<<loc>>!!",
            .sep = " "
        ),
        glue::as_glue("who is at home? I am at home!")
    )
})


# roll_middle() tests -----------------------------------------------------

test_that("roll_middle() works", {
    x <- c(7, 14, 21, 25)
    expect_equal(roll_middle(x, 0), c(3.5, 10.5, 17.5, 23))
    expect_equal(roll_middle(x, 0, "min"), c(3.5, 10.5, 17.5, 23))
    expect_equal(roll_middle(x, 30, "max"), c(10.5, 17.5, 23, 27.5))
})
