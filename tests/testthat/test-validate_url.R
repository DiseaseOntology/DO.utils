test_that("validate_url() works", {
    check_internet()

    res <- validate_url("disease-ontology.org")
    expect_equal(
        dplyr::select(res, -response),
        tibble::tibble(
            url = "disease-ontology.org",
            status = "Success",
            status_code = 200L,
            redirect_url = "https://disease-ontology.org/"
        )
    )
    expect_equal(class(res$response[[1]]), "response")
})

test_that("validate_url() works for R errors", {
    expect_equal(
        validate_url("https://this-url-doesnt-exist.none"),
        tibble::tibble(
            url = "https://this-url-doesnt-exist.none",
            status = "R_error",
            status_code = NA_integer_,
            redirect_url = NA_character_,
            exception = "Could not resolve host: this-url-doesnt-exist.none"
        )
    )
})

