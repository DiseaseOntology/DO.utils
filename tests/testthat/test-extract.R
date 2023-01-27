# extract_pmid() ----------------------------------------------------------

##### DATA #####
elink_list_full <- readRDS("data/elink_list_full.rds") # 2 elements both w/results
elink_list <- readRDS("data/elink_list.rds") # 3 elements, only 1st w/results
elink <- elink_list[[1]]
elink0 <- elink_list[[2]]

##### TESTS #####
test_that("extract_pmid elink method works", {
    expect_error(extract_pmid(elink0), class = "no_result")
    expect_equal(extract_pmid(elink), c("35380658", "33594374"))
})


test_that("extract_pmid elink_list method works", {
    expect_snapshot(extract_pmid(elink_list)) # default = warning
    expect_snapshot(extract_pmid(elink_list, no_result = "message"))
    expect_error( #removed snapshot due to tidyverse/purrr#945, v1.0.0)
        extract_pmid(elink_list, no_result = "error"),
        regexp = "35504280: 0 PubMed citedby results"
    )
    expect_equal(
        extract_pmid(elink_list, no_result = "none"),
        list("34755882" = c("35380658", "33594374"))
    )
})


test_that("extract_pmid.elink_list does NOT produce condition when all elements have results", {
    expect_condition(regexp = NA, extract_pmid(elink_list_full))
    expect_condition(
        regexp = NA,
        extract_pmid(elink_list_full, no_result = "message")
    )
    expect_condition(
        regexp = NA,
        extract_pmid(elink_list_full, no_result = "error")
    )
})


# extract_class_axiom() ---------------------------------------------------

test_that("extract_class_axiom() errors appropriately", {
    expect_error(
        extract_class_axiom('blah', NA, NA),
        regexp = "At least one"
    )
    expect_error(
        extract_class_axiom('blah', entire = 'fake_class', NA),
        regexp = "entire.*fake_class"
    )
    expect_error(
        extract_class_axiom('blah', NA, anon_only = 'fake_class'),
        regexp = "anon_only.*fake_class"
    )
    expect_error(extract_class_axiom('blah'), regexp = "ofn.*file")
})

