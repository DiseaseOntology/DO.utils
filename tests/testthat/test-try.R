test_that("Ensure try_url() uses DO.utils' user agent", {
    check_internet()
    resp <- try_url("disease-ontology.org")

    expect_equal(resp$request$options$useragent, DO_agent("full"))
})
