check_website <- function(url) {
    if (!system2("ping", paste0("-c1 ", url), stdout = FALSE) == 0) {
        skip(paste0(url, " is not available."))
    }
}

tdir <- tempdir()

test_that("download_obo_ontology() works works", {
    skip_on_cran()
    check_website("www.obofoundry.org")
    expect_true(
        file.exists(
            download_obo_ontology("bfo", dest_dir = tdir, quiet = TRUE)
        )
    )
})
