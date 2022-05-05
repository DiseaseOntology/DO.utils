# 3 elements: 1st = results, 2nd = no results, 3rd = rename of 2nd
elink_list <- readRDS("data/elink_list.rds")
elink <- elink_list[[1]]
elink0 <- elink_list[[2]]

test_that("extract_pmid elink method works", {
    expect_error(extract_pmid(elink0), class = "no_result")
    expect_equal(extract_pmid(elink), c("35380658", "33594374"))
})

test_that("extract_pmid elink_list method works", {
    expect_snapshot(extract_pmid(elink_list)) # default = warning
    expect_snapshot(extract_pmid(elink_list, no_result = "message"))
    expect_snapshot(
        error = TRUE,
        extract_pmid(elink_list, no_result = "error")
    )
    expect_equal(
        extract_pmid(elink_list, no_result = "none"),
        list("34755882" = c("35380658", "33594374"))
    )
})
