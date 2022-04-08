# data
dom <- c(
    # not duplicated
    "www.betterhealth.vic.gov.au", "journals.lww.com",
    # duplicated +/- www
    "omim.org", "www.omim.org",
    "rarediseases.org", "www.rarediseases.org"
)

url <- c(
    "https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/barmah-forest-virus-disease",
    "https://journals.lww.com/prsgo/fulltext/2020/12000/basal_cell_carcinoma_with_sebaceous.50.aspx",
    "http://omim.org/entry/608600",
    "https://rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
    "https://www.omim.org/entry/610984",
    "http://www.rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome"
)


test_that("standardize_www_duplicate() works for bare domains", {
    dom_rm <- c(
        "www.betterhealth.vic.gov.au", "journals.lww.com",
        "omim.org", "omim.org",
        "rarediseases.org", "rarediseases.org"
    )
    expect_equal(standardize_www_duplicate(dom), dom_rm)

    dom_add <- c(
        "www.betterhealth.vic.gov.au", "journals.lww.com",
        "www.omim.org", "www.omim.org",
        "www.rarediseases.org", "www.rarediseases.org"
    )
    expect_equal(standardize_www_duplicate(dom, how = "add"), dom_add)
})

test_that("standardize_www_duplicate() works for URLs", {
    url_rm <- c(
        "https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/barmah-forest-virus-disease",
        "https://journals.lww.com/prsgo/fulltext/2020/12000/basal_cell_carcinoma_with_sebaceous.50.aspx",
        "http://omim.org/entry/608600",
        "https://rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
        "https://omim.org/entry/610984",
        "http://rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome"
    )
    expect_equal(standardize_www_duplicate(url), url_rm)

    url_add <- c(
        "https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/barmah-forest-virus-disease",
        "https://journals.lww.com/prsgo/fulltext/2020/12000/basal_cell_carcinoma_with_sebaceous.50.aspx",
        "http://www.omim.org/entry/608600",
        "https://www.rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
        "https://www.omim.org/entry/610984",
        "http://www.rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome"
    )
    expect_equal(standardize_www_duplicate(url, how = "add"), url_add)
})
