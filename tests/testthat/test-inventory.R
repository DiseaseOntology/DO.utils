.tbl <- tibble::tribble(
    ~ row, ~ x, ~ p, ~ y,
    1L, "a", "skos:exactMatch", "A",
    2L, "a", "skos:exactMatch", "B",
    3L, "b", "oboInOwl:hasDbXref", "A",
    4L, "b", NA_character_, NA_character_,
    5L, "c", "oboInOwl:hasDbXref|skos:broadMatch", "B",
    6L, "c", "oboInOwl:hasDbXref|skos:exactMatch", "C"
)

test_that("multimap() works with default include & ignore predicates", {
    expect_equal(multimaps(.tbl$x, .tbl$p, .tbl$y), c(T, T, F, F, T, T))
    expect_equal(multimaps(.tbl$y, .tbl$p, .tbl$x), c(T, T, T, F, T, F))
})

test_that("multimap() works including only skos:(exact|close)Match, excludes oboInOwl:hasDbXref", {
    expect_equal(
        multimaps(
            .tbl$x, .tbl$p, .tbl$y,
            include_pred = c("skos:exactMatch", "skos:closeMatch"),
            ignore_pred = c("skos:narrowMatch", "skos:broadMatch", "skos:relatedMatch", "oboInOwl:hasDbXref")
        ),
        c(T, T, F, F, F, F)
    )
    expect_equal(
        multimaps(
            .tbl$y, .tbl$p, .tbl$x,
            include_pred = c("skos:exactMatch", "skos:closeMatch"),
            ignore_pred = c("skos:narrowMatch", "skos:broadMatch", "skos:relatedMatch", "oboInOwl:hasDbXref")
        ),
        c(F, F, F, F, F, F)
    )
})

test_that("multimaps() returns all `FALSE` when either input has only `NA`", {
    .tbl <- dplyr::filter(.tbl, dplyr::row_number() < 4) %>%
        dplyr::mutate(y = NA)

    expect_equal(multimaps(.tbl$x, .tbl$p, .tbl$y), rep(FALSE, 3))
    expect_equal(multimaps(.tbl$x, .tbl$p, .tbl$y), rep(FALSE, 3))
})

test_that("multimaps() errors for unknown/missing predicates", {
    .tbl$p[2] <- "random:mapping"
    .tbl$p[c(1,3)] <- NA
    expect_error(
        multimaps(.tbl$x, .tbl$p, .tbl$y),
        regexp = "NA.*1,3.*random:mapping.*2"
    )
})
