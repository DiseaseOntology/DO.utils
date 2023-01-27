test_that("read_term_file() works", {
    term_file <- "data/test_terms.txt"
    result <- tibble::tibble(
        id = c(
            "http://purl.obolibrary.org/obo/HP_0000023", "HP:0000037",
            "http://purl.obolibrary.org/obo/HP_0000083", "HP:0000090", "HP:0000219"
        ),
        manual_label = c(
            "Inguinal hernia", "Male pseudohermaphroditism",
            "Renal insufficiency", "Nephronophthisis",
            "Thin upper lip vermilion"
        )
    )

    expect_equal(read_term_file(term_file), result)
})
