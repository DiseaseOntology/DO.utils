.tbl <- tibble::tribble(
    ~ row, ~ x, ~ p, ~ y,
    1L, "a", "skos:exactMatch", "A",
    2L, "a", "skos:exactMatch", "B",
    3L, "b", "oboInOwl:hasDbXref", "A",
    4L, "b", NA_character_, NA_character_,
    5L, "c", "oboInOwl:hasDbXref|skos:broadMatch", "B",
    6L, "c", "oboInOwl:hasDbXref|skos:exactMatch", "C"
)

test_that("multimap() works with `include_hasDbXref = FALSE`", {
    expect_equal(
        multimaps(.tbl$x, .tbl$p, .tbl$y, include_hasDbXref = FALSE),
        c(T, T, F, F, F, F)
    )
    expect_equal(
        multimaps(.tbl$y, .tbl$p, .tbl$x, include_hasDbXref = FALSE),
        c(F, F, F, F, F, F)
    )
})

test_that("multimap() works with `include_hasDbXref = TRUE`", {
    expect_equal(
        multimaps(.tbl$x, .tbl$p, .tbl$y, include_hasDbXref = T),
        c(T, T, F, F, T, T)
    )
    expect_equal(
        multimaps(.tbl$y, .tbl$p, .tbl$x, include_hasDbXref = T),
        c(T, T, T, F, T, F)
    )
})

test_that("multimaps() works when either input has only `NA`", {
    x <- c("OMIM:PS136520", "OMIM:136520", "OMIM:609218")
    pred <- c(NA_character_, NA_character_, NA_character_)
    y <- c(NA_character_, NA_character_, NA_character_)

    expect_equal(multimaps(x, pred, y), rep(FALSE, 3))
    expect_equal(multimaps(y, pred, x), rep(FALSE, 3))
})
