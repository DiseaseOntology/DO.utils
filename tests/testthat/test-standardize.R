# data
dom <- c(
    ##### not duplicated #####
    # single entry
    "www.betterhealth.vic.gov.au", "journals.lww.com",
    # multiple entry
    "www.orpha.net", "www.orpha.net",
    "en.wikipedia.org", "en.wikipedia.org",
    # duplicated (+/- www)
    "omim.org", "www.omim.org",
    "rarediseases.org", "www.rarediseases.org"
)

url <- c(
    "https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/barmah-forest-virus-disease",
    "https://journals.lww.com/prsgo/fulltext/2020/12000/basal_cell_carcinoma_with_sebaceous.50.aspx",
    "http://omim.org/entry/608600",
    "https://www.orpha.net/data/patho/GB/uk-TCII.pdf",
    "http://en.wikipedia.org/wiki/IgM",
    "https://rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
    "https://www.omim.org/entry/610984",
    "http://www.rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome",
    "http://en.wikipedia.org/wiki/IgM",
    "https://www.orpha.net/data/patho/GB/uk-AMLM7.pdf"
)


test_that("standardize_www_duplicate() works for bare domains", {
    dom_rm <- c(
        "www.betterhealth.vic.gov.au", "journals.lww.com",
        "www.orpha.net", "www.orpha.net",
        "en.wikipedia.org", "en.wikipedia.org",
        "omim.org", "omim.org",
        "rarediseases.org", "rarediseases.org"
    )
    expect_equal(standardize_www_duplicate(dom), dom_rm)

    dom_add <- c(
        "www.betterhealth.vic.gov.au", "journals.lww.com",
        "www.orpha.net", "www.orpha.net",
        "en.wikipedia.org", "en.wikipedia.org",
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
        "https://www.orpha.net/data/patho/GB/uk-TCII.pdf",
        "http://en.wikipedia.org/wiki/IgM",
        "https://rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
        "https://omim.org/entry/610984",
        "http://rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome",
        "http://en.wikipedia.org/wiki/IgM",
        "https://www.orpha.net/data/patho/GB/uk-AMLM7.pdf"
    )
    expect_equal(standardize_www_duplicate(url), url_rm)

    url_add <- c(
        "https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/barmah-forest-virus-disease",
        "https://journals.lww.com/prsgo/fulltext/2020/12000/basal_cell_carcinoma_with_sebaceous.50.aspx",
        "http://www.omim.org/entry/608600",
        "https://www.orpha.net/data/patho/GB/uk-TCII.pdf",
        "http://en.wikipedia.org/wiki/IgM",
        "https://www.rarediseases.org/rare-diseases/human-monocytic-ehrlichiosis-hme/",
        "https://www.omim.org/entry/610984",
        "http://www.rarediseases.org/search/rdbdetail_abstract.html?disname=LADD%20Syndrome",
        "http://en.wikipedia.org/wiki/IgM",
        "https://www.orpha.net/data/patho/GB/uk-AMLM7.pdf"
    )
    expect_equal(standardize_www_duplicate(url, how = "add"), url_add)
})
